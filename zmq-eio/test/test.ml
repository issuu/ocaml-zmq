open OUnit

let sleepf env secs = Eio.Time.sleep (Eio.Stdenv.clock env) secs

let setup ~sw env =
  let make ctx tpe =
    let s = Zmq.Socket.create ctx tpe in
    Zmq.Socket.set_receive_high_water_mark s 1;
    Zmq.Socket.set_send_high_water_mark s 2;
    s
  in
  let ctx = Zmq.Context.create () in
  let s1 = make ctx Zmq.Socket.pair in
  let s2 = make ctx Zmq.Socket.pair in
  let endpoint = "inproc://test"  in
  Zmq.Socket.bind s1 endpoint;
  Zmq.Socket.connect s2 endpoint;
  (* Sleep a bit *)
  sleepf env 0.0001;
  (ctx, Zmq_eio.Socket.of_socket ~sw s1, Zmq_eio.Socket.of_socket ~sw s2)

let teardown ~sw:_ _env (ctx, s1, s2) =
  Zmq_eio.Socket.close s2;
  Zmq_eio.Socket.close s1;
  Zmq.Context.terminate ctx;
  ()

let all_ok l =
  Eio.Fiber.List.iter (fun f -> f ()) l

let send env ?(delay = 0.0) s count =
  let rec inner = function
    | 0 -> ()
    | n ->
      Zmq_eio.Socket.send s "test";
      sleepf env delay;
      inner (n - 1)
  in
  fun () -> inner count

let send_all env ?(delay = 0.0) s count =
  let rec inner = function
    | 0 -> ()
    | n ->
      Zmq_eio.Socket.send_all s ["test1"; "test2"; "test3"];
      sleepf env delay;
      inner (n - 1)
  in
  fun () -> inner count

let recv env ?(delay = 0.0) s count =
  let rec inner = function
    | 0 -> ()
    | n ->
      let _ = Zmq_eio.Socket.recv s in
      sleepf env delay;
      inner (n - 1)
  in
  fun () -> inner count

let recv_all env ?(delay = 0.0) s count =
  let rec inner = function
    | 0 -> ()
    | n ->
      let _ = Zmq_eio.Socket.recv_all s in
      sleepf env delay;
      inner (n - 1)
  in
  fun () -> inner count

(** Test functions *)
let test_setup_teardown ~sw:_ _env _s = ()

let count = 1000
(* Tests *)
let test_send_receive ~sw:_ env (_, s1, s2) =
  all_ok [
    send env s2 count;
    recv env s1 count;
  ]

let test_send_receive_all ~sw:_ env (_, s1, s2) =
  all_ok [
    send_all env s2 count;
    recv_all env s1 count;
  ]

let test_msend_mreceive ~sw:_ env (_, s1, s2) =
  all_ok [
    send env s2 count; send env s2 count; send env s2 count; send env s2 count;
    recv env s1 count; recv env s1 count; recv env s1 count; recv env s1 count;
  ]

let test_mix ~sw:_ env (_, s1, s2) =
  all_ok [
    send env s2 count; recv env s1 count;
    send env s1 count; recv env s2 count;
    send env s2 count; recv env s1 count;
    send env s1 count; recv env s2 count;
    send env s2 count; recv env s1 count;
  ]

let test_slow_send ~sw:_ env (_, s1, s2) =
  all_ok [
    recv env ~delay:0.0001 s2 count;
    send env s1 (count / 5);
    send env s1 (count / 5);
    send env s1 (count / 5);
    send env s1 (count / 5);
    send env s1 (count / 5);
  ]

let test_slow_receive ~sw:_ env (_, s1, s2) =
  all_ok [
    send env ~delay:0.0001 s2 count;
    recv env s1 (count / 5);
    recv env s1 (count / 5);
    recv env s1 (count / 5);
    recv env s1 (count / 5);
    recv env s1 (count / 5);
  ]

let test_slow_mix1 ~sw:_ env (_, s1, s2) =
  all_ok [
    send env ~delay:0.0001 s2 count; recv env ~delay:0.0002 s1 count;
    send env ~delay:0.0001 s1 count; recv env ~delay:0.0002 s2 count;
    send env ~delay:0.0001 s2 count; recv env ~delay:0.0002 s1 count;
    send env ~delay:0.0001 s1 count; recv env ~delay:0.0002 s2 count;
  ]

let test_slow_mix2 ~sw:_ env (_, s1, s2) =
  all_ok [
    send env ~delay:0.0002 s2 count; recv env ~delay:0.0001 s1 count;
    send env ~delay:0.0002 s1 count; recv env ~delay:0.0001 s2 count;
    send env ~delay:0.0002 s2 count; recv env ~delay:0.0001 s1 count;
    send env ~delay:0.0002 s1 count; recv env ~delay:0.0001 s2 count;
  ]


let suite () =
  let bracket test =
    let f sw env =
      let s = setup ~sw env in
      match test ~sw env s with
      | v -> teardown ~sw env s; v
      | exception e -> teardown ~sw env s; raise e
    in
    fun () -> Eio_main.run (fun env ->
      Eio.Switch.run (fun sw -> f sw env))
  in

  __MODULE__ >::: [
    "test_setup_teardown"   >:: bracket test_setup_teardown;
    "test_send_receive"     >:: bracket test_send_receive;
    "test_msend_mreceive"   >:: bracket test_msend_mreceive;
    "test_mix"              >:: bracket test_mix;
    "test_slow_send"        >:: bracket test_slow_send;
    "test_slow_receive"     >:: bracket test_slow_receive;
    "test_slow_mix"         >:: bracket test_slow_mix1;
    "test_slow_mix"         >:: bracket test_slow_mix2;
    "test_send_receive_all" >:: bracket test_send_receive_all;
  ]


let () =
  run_test_tt_main (suite ()) |> ignore
