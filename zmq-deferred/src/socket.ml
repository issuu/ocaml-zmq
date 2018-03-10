module Make(T: Deferred.T) = struct
  open T
  open Deferred.Infix
  exception Retry
  type 'a t =
    { socket : 'a ZMQ.Socket.t;
      fd : Fd.t;
      senders : (unit -> unit) Queue.t;
      receivers : (unit -> unit) Queue.t;
    }

 let of_socket: 'a ZMQ.Socket.t -> 'a t = fun socket ->
    let fd = Fd.create (ZMQ.Socket.get_fd socket) in
    { socket; fd;
      senders = Queue.create ();
      receivers = Queue.create ();
    }

  (** The event loop repeats acting on events as long as there are
      sends or receives to be processed.
      According to the zmq specification, send and receive may update the event,
      and the fd can only be trusted after reading the status of the socket.
  *)
  let rec event_loop t =
    let open ZMQ.Socket in

    let process queue =
      let f = Queue.peek queue in
      try
        f ();
        (* Success, pop the sender *)
        Queue.pop t.senders |> ignore
      with
      | Retry ->
        ()
    in
    match events t.socket, Queue.is_empty t.senders, Queue.is_empty t.receivers with
    | _, true, true -> Deferred.return ()
    | Poll_error, _, _ -> failwith "Cannot poll socket"
    | Poll_in_out, false, _
    | Poll_out, false, _ ->
      (* Prioritize send's to keep network busy *)
      process t.senders;
      event_loop t
    | Poll_in_out, _, false
    | Poll_in, _, false ->
      process t.senders;
      event_loop t
    | Poll_in, _, true
    | Poll_out, true, _
    | No_event, _, _ ->
      Fd.wait_readable t.fd >>= fun () ->
      event_loop t

  type op = Send | Receive
  let post: _ t -> op -> (_ ZMQ.Socket.t -> 'a) -> 'a Deferred.t = fun t op f ->
    let is_running = Queue.is_empty t.senders && Queue.is_empty t.receivers in

    let cond = Condition.create () in
    let f' () =
      let res = match f t.socket with
        | v -> Ok v
        | exception Unix.Unix_error (Unix.EAGAIN, _, _) ->
          (* Signal try again *)
          raise Retry
        | exception exn -> Error exn
      in
      (* This may actually be a scheduling point *)
      Condition.wakeup cond res
    in
    let queue = match op with
      | Send -> t.senders
      | Receive -> t.receivers
    in
    Queue.push f' queue;

    (* Start a thread if none is running *)
    begin
      match is_running with
      | false -> ()
      | true -> Deferred.don't_wait_for (fun () -> event_loop t)
    end;

    (* Raise exceptions in callers context *)
    Condition.wait cond >>= function
    | Ok v -> Deferred.return v
    | Error exn -> Deferred.fail exn

  let to_socket t = t.socket

  let recv s = post s Receive (fun s -> ZMQ.Socket.recv ~block:false s)
  let send s m = post s Send (fun s -> ZMQ.Socket.send ~block:false s m)

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
    post s Receive (fun s -> ZMQ.Socket.recv_all ~block:false s)

  let send_all s parts =
    (* See the comment in recv_all. *)
    post s Send (fun s -> ZMQ.Socket.send_all ~block:false s parts)

  let close { socket ; fd } =
    ZMQ.Socket.close socket;
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
    let recv s = post s Receive (fun s -> ZMQ.Monitor.recv ~block:false s)
  end

end
