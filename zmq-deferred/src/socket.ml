module Make(T: Deferred.T) = struct
  open T
  open Deferred.Infix
  exception Retry
  type 'a t =
    { socket : 'a Zmq.Socket.t;
      fd : Fd.t;
      senders : (unit -> unit) Queue.t;
      receivers : (unit -> unit) Queue.t;
      condition : unit Condition.t;
      fd_condition : unit Condition.t;
      mutable closing : bool;
    }

  let to_string_hum t =
    let state = match (Zmq.Socket.events t.socket) with
      | Zmq.Socket.No_event -> "No_event"
      | Poll_in -> "Poll_in"
      | Poll_out -> "Poll_out"
      | Poll_in_out -> "Poll_in_out"
      | Poll_error -> "Poll_error"
      | exception _ -> "Closed"
    in
    Printf.sprintf "State: %s, Senders #%d, Receivers #%d"
      state
      (Queue.length t.senders)
      (Queue.length t.receivers)


  (** Small process that will notify of the fd changes *)
  let rec fd_monitor t =
    Condition.wait t.fd_condition >>= fun () ->
    match t.closing with
    | true -> Deferred.return ()
    | false -> begin
        Deferred.catch (fun () -> Fd.wait_readable t.fd) >>= fun _ ->
        Condition.signal t.condition ();
        match t.closing with
        | true -> Deferred.return ()
        | false -> fd_monitor t
      end

  (** The event loop repeats acting on events as long as there are
      sends or receives to be processed.
      According to the zmq specification, send and receive may update the event,
      and the fd can only be trusted after reading the status of the socket.
  *)
  let rec event_loop t =
    match t.closing with
    | true -> Deferred.return ()
    | false -> begin
        let open Zmq.Socket in
        let process queue =
          let f = Queue.peek queue in
          try
            f ();
            (* Success, pop the sender *)
            Queue.pop queue |> ignore
          with
          | Retry -> (* If f raised EAGAIN, dont pop the message *) ()
        in
        match events t.socket, Queue.is_empty t.senders, Queue.is_empty t.receivers with
        | _, true, true ->
          Condition.wait t.condition >>= fun () ->
          event_loop t
        | Poll_error, _, _ -> failwith "Cannot poll socket"
        (* Prioritize send's to keep network busy *)
        | Poll_in_out, false, _
        | Poll_out, false, _ ->
          process t.senders;
          event_loop t
        | Poll_in_out, _, false
        | Poll_in, _, false ->
          process t.receivers;
          event_loop t
        | Poll_in, _, true
        | Poll_out, true, _
        | No_event, _, _ ->
          Condition.signal t.fd_condition ();
          Condition.wait t.condition >>= fun () ->
          event_loop t
        | exception Unix.Unix_error(Unix.ENOTSOCK, "zmq_getsockopt", "") ->
          Deferred.return ()
      end

  let of_socket: 'a Zmq.Socket.t -> 'a t = fun socket ->
    let fd = Fd.create (Zmq.Socket.get_fd socket) in
    let t =
      { socket; fd;
        senders = Queue.create ();
        receivers = Queue.create ();
        condition = Condition.create ();
        fd_condition = Condition.create ();
        closing = false;
      }
    in
    Deferred.don't_wait_for (fun () -> event_loop t);
    Deferred.don't_wait_for (fun () -> fd_monitor t);
    t

  type op = Send | Receive
  let post: _ t -> op -> (_ Zmq.Socket.t -> 'a) -> 'a Deferred.t = fun t op f ->
    let f' mailbox () =
      let res = match f t.socket with
        | v -> Ok v
        | exception Unix.Unix_error (Unix.EAGAIN, _, _) ->
          (* Signal try again *)
          raise Retry
        | exception exn -> Error exn
      in
      Mailbox.send mailbox res
    in
    let queue = match op with
      | Send -> t.senders
      | Receive -> t.receivers
    in
    let mailbox = Mailbox.create () in
    let should_signal = Queue.is_empty queue in
    Queue.push (f' mailbox) queue;

    (* Wakeup the thread if the queue was empty *)
    begin
      match should_signal with
      | true -> Condition.signal t.condition ()
      | false -> ()
    end;

    Mailbox.recv mailbox >>= function
    | Ok v -> Deferred.return v
    | Error exn -> Deferred.fail exn

  let to_socket t = t.socket

  let recv s = post s Receive (fun s -> Zmq.Socket.recv ~block:false s)
  let send s m = post s Send (fun s -> Zmq.Socket.send ~block:false s m)

  let recv_msg s = post s Receive (fun s -> Zmq.Socket.recv_msg ~block:false s)
  let send_msg s m =
    post s Send (fun s -> Zmq.Socket.send_msg ~block:false s m)

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
    post s Receive (fun s -> Zmq.Socket.recv_all ~block:false s)

  let send_all s parts =
    (* See the comment in recv_all. *)
    post s Send (fun s -> Zmq.Socket.send_all ~block:false s parts)

  let recv_msg_all s =
    post s Receive (fun s -> Zmq.Socket.recv_msg_all ~block:false s)
  let send_msg_all s parts =
    post s Send (fun s -> Zmq.Socket.send_msg_all ~block:false s parts)

  let close t =
    t.closing <- true;
    Deferred.catch (fun () -> Fd.release t.fd) >>= fun _ ->
    Condition.signal t.fd_condition ();
    Condition.signal t.condition ();
    Zmq.Socket.close t.socket;
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
    let recv s = post s Receive (fun s -> Zmq.Monitor.recv ~block:false s)
  end

end
