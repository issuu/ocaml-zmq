open OUnit
open ZMQ


let string_of_event = function
  | Socket.No_event -> "No_event"
  | Poll_in -> "Poll_in"
  | Poll_out -> "Poll_out"
  | Poll_in_out -> "Poll_in_out"
  | Poll_error  -> "Poll_error"

(* Test fd, Send message, Test df, Read event, Test *)

type 'a socket = {
  socket: 'a Socket.t;
  fd: Unix.file_descr;
}

let can_read socket =
  match Unix.select [socket.fd] [] [] 0.001 with
  | [], _, _ -> false
  | _, _, _ -> true

let rec wait_readable socket =
  let rec inner = function
    | 0 -> false
    | n -> begin
        match can_read socket with
        | true -> true
        | false -> inner (n - 1)
      end
  in
  inner 10

let rec wait_not_readable socket =
  let rec inner = function
    | 0 -> false
    | n -> begin
        match can_read socket with
        | true ->
          Socket.events socket.socket |> ignore;
          inner (n - 1)
        | false -> true
      end
  in
  inner 10


(** Assert that t goes from last state to a new state when calling function f *)
let assert_state ?msg ?f socket state =
  assert_bool "Fd did not settle into a non readable state" (wait_not_readable socket);
  let prev_state = Socket.events socket.socket in

  begin
    match f with
    | None -> ()
    | Some f -> f ();
  end;

  (* Only expect the fd to become readable, if the state has a meaningfull change *)
  let expect_fd_change = match prev_state, state with
    | s, s' when s = s' -> false (* No state change *)
    | _, Socket.No_event (* From more to less *)
    | Poll_in_out, _ -> false
    | _ -> true
  in

  begin
    match expect_fd_change with
    | true ->
      begin
        match wait_readable socket with
        | true -> ()
        | false ->
          let msg = Printf.sprintf "Socket did not become readable: %s -> %s (expected %s)"
              (string_of_event prev_state)
              (string_of_event (Socket.events socket.socket))
              (string_of_event state)
          in
          assert_failure msg
      end
    | false -> ()
  end;

  assert_equal ?msg
    ~printer:string_of_event state (Socket.events socket.socket);
  assert_bool "Fd did not settle into a non readable state" (wait_not_readable socket);
  assert_equal ?msg
    ~printer:string_of_event state (Socket.events socket.socket);
  ()


let setup _ =
  let ctx = Context.create () in
  let create typ =
    let socket = ZMQ.Socket.create ctx typ in
    let fd = Socket.get_fd socket in
    Socket.set_send_high_water_mark socket 1;
    Socket.set_receive_high_water_mark socket 1;

    { socket; fd }
  in
  let push = create ZMQ.Socket.push in
  let pull = create ZMQ.Socket.pull in

  Unix.sleepf 1.0;

  (ctx, push, pull)

let teardown (ctx, push, pull) =
  Socket.close pull.socket;
  Socket.close push.socket;
  ZMQ.Context.terminate ctx


let test (ctx, push, pull) =
  assert_state push Socket.No_event;
  assert_state pull Socket.No_event;

  (* Bind to an endpoint*)
  let endpoint = "tcp://127.0.0.1:51235" in
  ZMQ.Socket.bind push.socket endpoint;
  Unix.sleepf 0.1;

  (* I would have expected the socket to go into a Pull_out state *)
  assert_state push Socket.No_event;

  assert_state push Socket.Poll_out
    ~f:(fun () -> Socket.connect pull.socket endpoint);

  assert_state pull Socket.No_event;

  (* Sending a message will change the state of pull *)
  assert_state pull Socket.Poll_in
    ~f:(fun () -> Socket.send push.socket "test msg");
  assert_state push Socket.Poll_out;

  (* Reading the message will change the state of pull, as there are no more messages *)
  assert_state pull Socket.No_event
    ~f:(fun () -> let (_: string) = Socket.recv pull.socket in ());
  assert_state push Socket.Poll_out;


  ()

let suite =
  "zmq" >:::
  [
    "block" >:: bracket setup test teardown
  ]
