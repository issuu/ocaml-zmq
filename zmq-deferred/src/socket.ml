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
    match ZMQ.Socket.events t.socket, dir with
    | ZMQ.Socket.Poll_in_out, _
    | Poll_out, `Write
    | Poll_in, `Read ->
      begin
        try
          f t.socket |> Deferred.return
        with
        | Unix.Unix_error (Unix.EAGAIN, _, _) -> wrap dir f t
        | Unix.Unix_error (Unix.EINTR, _, _) -> failwith "Break"
      end
    | Poll_error, _ -> failwith "Cannot poll socket"
    | Poll_in, _
    | Poll_out, _
    | No_event, _ ->
      (* Wait for the fd to become readable *)
      Fd.wait_readable t.fd >>= fun () ->
      wrap dir f t


  let recv s = wrap `Read (fun s -> ZMQ.Socket.recv ~block:false s) s
  let send s m = wrap `Read (fun s -> ZMQ.Socket.send ~block:false s m) s

  (** Recevie all message blocks. *)

  let recv_all s =
    (* The documentaton says that either all message parts are
       transmitted, or none.  Therefore the receiver end cannot notify
       userspace of a multipart message before all parts have been
       received.

       Also its not clear what the state of the socket would be after
       reception of the first part of a multipart message. It would
       not be unreasonable iff the state could change to No_event
       after reading the first part of a multi_message since no new
       messages are available.

       Also message parts needs to be received in order, so we must
       avoid interleaving here.
    *)
    wrap `Read (fun s -> ZMQ.Socket.recv_all ~block:false s) s

  let send_all s parts =
    (* See the comment on recv_all. Again, the state could change
       from Poll_out to No_event, as we are just filling up the first
       message, and according to the specification the message cannot
       be sent before all parts have been delivered.
    *)
    wrap `Write (fun s -> ZMQ.Socket.send_all ~block:false s parts) s

  let close { socket ; fd } =
    ZMQ.Socket.close socket;
    Fd.release fd;
    Deferred.return ()
end
