open OUnit
open Zmq


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

let wait_readable socket =
  let rec inner = function
    | 0 -> false
    | n -> begin
        match can_read socket with
        | true -> true
        | false -> inner (n - 1)
      end
  in
  inner 10

let wait_not_readable socket =
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


let setup typ_a typ_b _ =
  let ctx = Context.create () in
  let create typ =
    let socket = Zmq.Socket.create ctx typ in
    let fd = Socket.get_fd socket in
    Socket.set_send_high_water_mark socket 1;
    Socket.set_receive_high_water_mark socket 1;

    { socket; fd }
  in
  let a = create typ_a in
  let b = create typ_b in


  (ctx, a, b)

let teardown (ctx, push, pull) =
  Socket.close pull.socket;
  Socket.close push.socket;
  Zmq.Context.terminate ctx

let send ?more s () =
  Socket.send ?more s.socket ~block:false "test msg"

let recv s () =
  Socket.recv s.socket |> ignore


let test_unidir (_ctx, push, pull) =
  assert_state push Socket.No_event;
  assert_state pull Socket.No_event;

  (* Bind to an endpoint*)
  let endpoint = "inproc://fd_bidir_test" in
  Zmq.Socket.bind push.socket endpoint;

  (* I would have expected the socket to go into a Pull_out state *)
  assert_state push Socket.No_event;

  assert_state push Socket.Poll_out
    ~f:(fun () -> Socket.connect pull.socket endpoint);

  assert_state pull Socket.No_event;

  (* Sending a message will change the state of pull *)
  assert_state pull Socket.Poll_in ~f:(send push);
  assert_state push Socket.Poll_out;

  (* Reading the message will change the state of pull, as there are
     no more messages *)
  assert_state pull Socket.No_event ~f:(recv pull);
  assert_state push Socket.Poll_out;


  (* Fill up the queues  *)
  assert_state pull Socket.Poll_in ~f:(send push);
  assert_state push Socket.Poll_out;
  assert_state push Socket.No_event ~f:(send push);
  assert_state pull Socket.Poll_in;

  (* Reading a message will make the push socket ready for send again *)
  assert_state push Socket.Poll_out ~f:(recv pull);
  assert_state pull Socket.Poll_in;

  (* We can send more after the first send_more. *)
  assert_state push Socket.Poll_out ~f:(send ~more:true push);
  assert_state push Socket.Poll_out ~f:(send ~more:true push);

  (* Reading off the next message will yeild no more message *)
  assert_state pull Socket.No_event ~f:(recv pull);

  assert_state pull Socket.Poll_in ~f:(send ~more:false push);

  assert_state push Socket.No_event ~f:(send push);

  (* Starting to read the multipart message will not allow us to send more *)
  assert_state push Socket.No_event  ~f:(recv pull);
  assert_state pull Socket.Poll_in;
  assert_bool "Expected more messages" (Socket.has_more pull.socket);

  assert_state push Socket.No_event ~f:(recv pull);
  assert_state pull Socket.Poll_in;
  assert_bool "Expected more messages" (Socket.has_more pull.socket);

  (* Reading off the last message will make the push socket ready again *)
  assert_state push Socket.Poll_out ~f:(recv pull);
  assert_state pull Socket.Poll_in;
  assert_bool "no more messages expected" (Socket.has_more pull.socket |> not);

  ()

let test_bidir (_ctx, s_a, s_b) =
  (* Test how socket notifies state based on event avilability *)
  let endpoint = "inproc://fd_bidir_test" in
  Zmq.Socket.bind s_a.socket endpoint;
  Zmq.Socket.connect s_b.socket endpoint;

  (* all buffers are empty, all can send *)
  assert_state s_a Socket.Poll_out;
  assert_state s_b Socket.Poll_out;

  (* Push a message on a will not change a's state but b can receive *)
  assert_state s_b Socket.Poll_in_out ~f:(send s_a);
  assert_state s_a Socket.Poll_out;

  (* Send again, and s_a cannot send any more *)
  assert_state s_a Socket.No_event ~f:(send s_a);

  (* Send from b -> a, makes are readable *)
  assert_state s_a Socket.Poll_in ~f:(send s_b);
  assert_state s_b Socket.Poll_in_out;

  (* Send again => all queues are full.
     and b can only read *)
  assert_state s_a Socket.Poll_in ~f:(send s_b);
  assert_state s_b Socket.Poll_in;

  (* Reading a message on a changes b's state *)
  assert_state s_b Socket.Poll_in_out ~f:(recv s_a);
()


let suite =
  "zmq" >:::
  [
    "unidir" >:: bracket (setup Zmq.Socket.push Zmq.Socket.pull)
      test_unidir
      teardown;

    "bidir" >:: bracket (setup Zmq.Socket.pair Zmq.Socket.pair)
      test_bidir
      teardown;
  ]
