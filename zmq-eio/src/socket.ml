(** Eio based bindings for eio *)
exception Closed

type 'a t = {
  socket : 'a Zmq.Socket.t;
  fd : Unix.file_descr;
  senders : (unit -> unit) Queue.t;
  receivers : (unit -> unit) Queue.t;
  condition : Eio.Condition.t;
  mutex : Eio.Mutex.t;
  ready_condition: Eio.Condition.t;
  mutable thread : unit Eio.Promise.or_exn option; (* None indicates already closed *)
}

type 'a of_socket_args = sw:Eio.Switch.t -> 'a
type 'a deferred = 'a

(** invoke the first function on the queue, but only pop it if it does not raise EAGAIN *)
let process queue =
  match (Queue.peek queue) () with
  | () ->
    let (_: unit -> unit) = Queue.pop queue in
    ()
  | exception Unix.Unix_error (Unix.EAGAIN, _, _) ->
    (* If f raised EAGAIN, dont pop the message. *)
    (* This should never happen. If so, the queue could be replaced with a Eio.Stream for faster handling *)
    ()

let with_lock lock f =
  Eio.Mutex.lock lock;
  try
    let v = f () in
    Eio.Mutex.unlock lock;
    v
  with
  | e ->
    Eio.Mutex.unlock lock;
    raise e

let rec fd_monitor t =
  Eio.Condition.await_no_mutex t.ready_condition;
  Eio_unix.await_readable t.fd;
  with_lock t.mutex (fun () -> Eio.Condition.broadcast t.condition);
  fd_monitor t

let rec event_loop t =
  let inner () =
    match Zmq.Socket.events t.socket with
    | Zmq.Socket.Poll_error ->
      failwith "Cannot poll socket"
    | (Poll_in_out | Poll_in) when not (Queue.is_empty t.receivers) ->
      process t.receivers
    | (Poll_in_out | Poll_out) when not (Queue.is_empty t.senders) ->
      process t.senders
    | _ ->
      Eio.Condition.broadcast t.ready_condition;
      Eio.Condition.await t.condition t.mutex;
  in
  with_lock t.mutex (fun () -> inner ());
  match t.thread with
  | None when Queue.is_empty t.senders && Queue.is_empty t.receivers ->
    ()
  | _ ->
    event_loop t

let of_socket: ('a Zmq.Socket.t -> 'a t) of_socket_args = fun ~sw socket ->
  let fd = Zmq.Socket.get_fd socket in
  let t =
    { socket;
      fd;
      senders = Queue.create ();
      receivers = Queue.create ();
      mutex = Eio.Mutex.create ();
      condition = Eio.Condition.create ();
      ready_condition = Eio.Condition.create ();
      thread = None;
    }
  in
  let thread = Eio.Fiber.fork_promise ~sw (fun () ->
    Eio.Switch.run (fun sw ->
      Eio.Fiber.fork ~sw (fun () -> event_loop t);
      Eio.Fiber.fork_daemon ~sw (fun () -> fd_monitor t);
      ()
    );
  )
  in
  t.thread <- Some thread;
  t

let to_socket t =
  t.socket

(** Stop the deamon thread, and ensure that all sends and receives has been handled *)
let close t =
  let thread = match t.thread with
    | None -> failwith "Socket already closed"
    | Some t -> t
  in
  with_lock t.mutex (fun () -> t.thread <- None; Eio.Condition.broadcast t.condition);
  let _e = Eio.Promise.await_exn thread in
  Zmq.Socket.close t.socket;
  ()


let request t queue f =
  let () =
    match t.thread with
    | None -> raise Closed
    | Some _ -> ()
  in
  let (pt, pu) = Eio.Promise.create ~label:"Zmq" () in
  let f () =
    let v = f () in
    Eio.Promise.resolve pu v
  in
  with_lock t.mutex (fun () -> Queue.push f queue; Eio.Condition.broadcast t.condition);
  Eio.Promise.await pt

let send t message =
  request t t.senders (fun () -> Zmq.Socket.send ~block:false t.socket message)

let send_msg t message =
  request t t.senders (fun () -> Zmq.Socket.send_msg ~block:false t.socket message)

let send_all t messages =
  request t t.senders (fun () -> Zmq.Socket.send_all ~block:false t.socket messages)

let send_msg_all t messages =
  request t t.senders (fun () -> Zmq.Socket.send_msg_all ~block:false t.socket messages)

let recv t =
  request t t.receivers (fun () -> Zmq.Socket.recv ~block:false t.socket)

let recv_msg t =
  request t t.receivers (fun () -> Zmq.Socket.recv_msg ~block:false t.socket)

let recv_all t =
  request t t.receivers (fun () -> Zmq.Socket.recv_all ~block:false t.socket)

let recv_msg_all t =
  request t t.receivers (fun () -> Zmq.Socket.recv_msg_all ~block:false t.socket)

module Router = struct
  type id_t = string

  let id_of_string t = t

  let recv t =
    match recv_all t with
    | id :: message -> (id, message)
    | _ -> assert false

  let send t id message =
    send_all t (id :: message)
end

module Monitor = struct
  let recv t = request t t.receivers (fun () -> Zmq.Monitor.recv ~block:false t.socket)
end
