open Base
open Async_kernel
open Async_unix

module Socket = struct
  exception Break_event_loop [@@deriving sexp_of]
  exception Retry [@@deriving sexp_of]

  type 'a t =
    { socket : 'a ZMQ.Socket.t sexp_opaque
    ; fd : Fd.t
    }
  [@@deriving sexp_of]

  let to_socket t = t.socket

  let of_socket socket =
    let fd =
      Fd.create
        (Fd.Kind.Socket `Active)
        (ZMQ.Socket.get_fd socket)
        (Info.of_string "<zmq>")
    in
    { socket; fd }

  let zmq_event socket ~f =
    match ZMQ.Socket.events socket with
    | ZMQ.Socket.No_event -> raise Retry
    | Poll_in
    | Poll_out
    | Poll_in_out -> f socket
    | Poll_error -> assert false
    | exception Unix.Unix_error (Unix.EAGAIN, _, _) -> raise Retry
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> raise Break_event_loop

  let wrap kind (f : _ ZMQ.Socket.t -> _) { socket ; fd } =
    let io_loop () =
      Fd.ready_to fd kind >>= function
      | `Bad_fd -> assert false (* It's an fd we created. shouldn't be bad *)
      | `Closed -> failwith "fd is closed"
      | `Ready ->
        In_thread.syscall_exn ~name:"<wrap>" (fun () ->
            (* Check for zeromq events *)
            match ZMQ.Socket.events socket with
            | ZMQ.Socket.No_event -> raise Retry
            | Poll_in
            | Poll_out
            | Poll_in_out -> f socket
            (* This should not happen as far as I understand *)
            | Poll_error -> assert false
            (* Not ready *)
            | exception Unix.Unix_error (Unix.EAGAIN, _, _) ->
              raise Retry
            (* We were interrupted so we need to start all over again *)
            | exception Unix.Unix_error (Unix.EINTR, _, _) ->
              raise Break_event_loop
          )
    in
    let rec idle_loop () =
      (* why are we running things in a monitor here? *)
      try_with ~extract_exn:true (fun () -> return (f socket)) >>= function
      | Ok x -> return x
      | Error (Unix.Unix_error (Unix.EINTR, _, _)) -> idle_loop ()
      | Error (Unix.Unix_error (Unix.EAGAIN, _, _)) ->
        begin try_with ~extract_exn:true io_loop >>= function
          | Ok x -> return x
          | Error Retry
          | Error Break_event_loop -> idle_loop ()
          | Error x -> raise x
        end
      | Error x -> raise x
    in
    idle_loop ()

  let recv s = wrap `Read (fun s -> ZMQ.Socket.recv ~block:false s) s

  let send s m = wrap `Write (fun s -> ZMQ.Socket.send ~block:false s m) s

  let recv_all s =
    wrap `Read (fun s -> ZMQ.Socket.recv_all ~block:false s) s

  let send_all s parts =
    wrap `Write (fun s -> ZMQ.Socket.send_all ~block:false s parts) s

  let close { socket ; fd } =
    ZMQ.Socket.close socket;
    Fd.close fd
end
