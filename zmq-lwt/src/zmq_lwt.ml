let (>>=) = Lwt.(>>=)

module Socket = struct

  type 'a t = {
    socket : 'a ZMQ.Socket.t;
    fd : Lwt_unix.file_descr;
  }

  exception Break_event_loop

  let of_socket socket = {
    socket;
    fd = Lwt_unix.of_unix_file_descr ~blocking:false ~set_flags:false (ZMQ.Socket.get_fd socket);
  }

  let to_socket s = s.socket

  (* Wrap possible exceptions and events which can occur in a ZeroMQ call *)
  let wrap f s =
    let io_loop () =
      Lwt_unix.wrap_syscall Lwt_unix.Read s.fd (
        fun () ->
          (* Check for zeromq events *)
          match ZMQ.Socket.events s.socket with
          | ZMQ.Socket.No_event -> raise Lwt_unix.Retry
          | ZMQ.Socket.Poll_in
          | ZMQ.Socket.Poll_out
          | ZMQ.Socket.Poll_in_out -> f s.socket
          (* This should not happen as far as I understand *)
          | ZMQ.Socket.Poll_error -> assert false
          (* Not ready *)
          | exception Unix.Unix_error (Unix.EAGAIN, _, _) -> raise Lwt_unix.Retry
          (* We were interrupted so we need to start all over again *)
          | exception Unix.Unix_error (Unix.EINTR, _, _) -> raise Break_event_loop
      )
    in
    let rec idle_loop () =
      Lwt.catch
        (fun () -> Lwt.wrap1 f s.socket)
        (function
          | Unix.Unix_error ( Unix.EAGAIN, _, _) ->
            Lwt.catch io_loop
              (function
                | Break_event_loop -> idle_loop ()
                | exn -> Lwt.fail exn)
          | Unix.Unix_error (Unix.EINTR, _, _) ->
            idle_loop ()
          | exn -> Lwt.fail exn)
    in
    idle_loop ()

  let recv s =
    wrap (fun s -> ZMQ.Socket.recv ~block:false s) s

  let send ?more s m =
    wrap (fun s -> ZMQ.Socket.send ?more ~block:false s m) s

  let recv_all s =
    wrap (fun s -> ZMQ.Socket.recv_all ~block:false s) s

  let send_all s parts =
    wrap (fun s -> ZMQ.Socket.send_all ~block:false s parts) s

  module Router = struct
    type id_t = string

    let id_of_string id = id

    let recv s =
      recv_all s >>= function
      | id :: message -> Lwt.return (id, message)
      | _ -> assert false

    let send s id message =
      send_all s (id :: message)
  end
end

module Monitor = struct
  let recv s = Socket.wrap (fun s -> ZMQ.Monitor.recv ~block:false s) s
end
