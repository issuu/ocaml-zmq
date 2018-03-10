module Make(Deferred: Deferred.T) = struct

  open Deferred
  exception Break_event_loop
  exception Retry

  type 'a t =
    { socket : 'a ZMQ.Socket.t;
      fd : Fd.t;
    }

  let to_socket t = t.socket

  let of_socket: 'a ZMQ.Socket.t -> 'a t = fun socket ->
    let fd = Fd.create (ZMQ.Socket.get_fd socket) in
    { socket; fd }

  let rec wrap dir f t =
    (* Reading Socket.events resets the fd, and
       fd will become readable when Socket.events changes. *)

    match ZMQ.Socket.events t.socket, dir with
    | ZMQ.Socket.Poll_in_out, _
    | Poll_out, `Write
    | Poll_in, `Read ->
      begin
        try
          f t.socket |> Deferred.return
        with
        | Unix.Unix_error (Unix.EAGAIN, _, _) ->
          (* This should actually never happen, as we have just validated
             that the operation would be non-blocking *)
          wrap dir f t
      end
    | Poll_error, _ -> failwith "Cannot poll socket"
    | Poll_in, `Write
    | Poll_out, `Read
    | No_event, _ ->
      (* Wait for the fd to become readable *)
      Fd.wait_readable t.fd >>= fun () ->
      wrap dir f t


  let recv s = wrap `Read (fun s -> ZMQ.Socket.recv ~block:false s) s
  let send s m = wrap `Read (fun s -> ZMQ.Socket.send ~block:false s m) s

  (** Recevie all message blocks. *)

  let recv_all s =
    (* The documentaton says that either all message parts are
       transmitted, or none. So once a message becomes available, all
       parts can be read wothout blocking.

       Also receiving a multipart message must not be interleaved with
       another receving thread on the same socket.

       We could have a read-mutex and a write mutex in order to limit
       potential starvation of other threads while reading large
       multipart messages.

    *)
    wrap `Read (fun s -> ZMQ.Socket.recv_all ~block:false s) s

  let send_all s parts =
    (* See the comment in recv_all. *)
    wrap `Write (fun s -> ZMQ.Socket.send_all ~block:false s parts) s

  let close { socket ; fd } =
    ZMQ.Socket.close socket;
    Fd.release fd;
    Deferred.return ()


  module Router = struct
    type id_t = string

    let id_of_string t = t

    let recv s =
      recv_all s >>= function
      | id :: message -> Deferred.return (id, message)
      | _ -> assert false

    let send s id message =
      send_all s (id :: message)
  end

  module Monitor = struct
    let recv s = wrap `Read (fun s -> ZMQ.Monitor.recv ~block:false s) s
  end

end
