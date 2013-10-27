open OUnit

open ZMQ
open ZMQ.Socket
open ZMQ.Poll

let debug fmt =
  Printf.ksprintf (fun s -> print_endline s; flush stdout) fmt

let sleep t = ignore(Unix.select [] [] [] ((float t) /. 1000.0))

let dump_events l =
  let f  = function
    | None -> "None"
    | Some In -> "In"
    | Some Out -> " Out"
    | Some In_out -> "In/Out"
  in
  let l = Array.to_list (Array.map f l) in
  "[|" ^ (String.concat "; " l) ^ "|]"

let test_options () =
  let socket =
    let ctx = init () in
    let s = create ctx push in
    s
  in

  let test_set_get msg setter getter socket v =
    let default = getter socket in
    setter socket v;
    assert_equal ~msg ~printer:string_of_int v (getter socket);
    setter socket default;
    assert_equal ~msg default (getter socket);
    ()
  in

  test_set_get "Highwatermark" set_recevice_high_water_mark get_recevice_high_water_mark socket 1235;
  test_set_get "Affinity" set_affinity get_affinity socket 3;
  test_set_get "Receive timeout" set_receive_timeout get_receive_timeout socket 1000;

  ()

let test_monitor () =
  let event_to_string event =
    let open ZMQ.Monitor in
    match event with
    | Connected (addr, _fd) -> Printf.sprintf "Connect: %s" addr
    | Connect_delayed (addr, error_no, error_text) -> Printf.sprintf "Connect delayed: %s %d %s" addr error_no error_text
    | Connect_retried (addr, interval) -> Printf.sprintf "retried: %s %d" addr interval
    | Listening (addr, _fd) -> Printf.sprintf "Listening: %s" addr
    | Bind_failed (addr, error_no, error_text) -> Printf.sprintf "Bind failed: %s %d %s" addr error_no error_text
    | Accepted (addr, _fd) -> Printf.sprintf "Accepted: %s" addr
    | Accept_failed (addr, error_no, error_text) -> Printf.sprintf "Accept failed: %s %d %s" addr error_no error_text
    | Closed (addr, _fd) -> Printf.sprintf "Closed: %s" addr
    | Close_failed (addr, error_no, error_text) -> Printf.sprintf "Close failed: %s %d %s" addr error_no error_text
    | Disconnected (addr, _fd) -> Printf.sprintf "Disconnected: %s" addr
  in

  let ctx = init () in
  let endpoint = "tcp://127.0.0.1:51234" in
  let s1 = ZMQ.Socket.create ctx ZMQ.Socket.pair
  and s2 = ZMQ.Socket.create ctx ZMQ.Socket.pair in
  let m1 = let mon_t = ZMQ.Monitor.create s1 in
           ZMQ.Monitor.connect ctx mon_t
  and m2 = let mon_t = ZMQ.Monitor.create s2 in
           ZMQ.Monitor.connect ctx mon_t
  in
  (* Start generating events *)
  ZMQ.Socket.bind s1 endpoint;
  ZMQ.Socket.connect s2 endpoint;
  sleep 100;

  ZMQ.Socket.close s2;
  ZMQ.Socket.close s1;
  sleep 100;


  let assert_events socket events =
    let printer x = x in
    let rec cmp str1 str2 =
      match String.length str1 <= String.length str2 with
      | true -> String.sub str2 0 (String.length str1) = str1
      | false -> cmp str2 str1
    in
    let assert_event event =
      let received =
        (event_to_string (ZMQ.Monitor.recv ~opt:ZMQ.Socket.R_no_block socket));
      in
      assert_equal ~msg:"Wrong event received" ~printer ~cmp
        (Printf.sprintf "%s: %s" event endpoint) received
      in
    List.iter assert_event events
  in
  assert_events m1 [ "Listening"; "Accepted"; "Closed" ];
  assert_events m2 [ "Connect delayed"; "Connect" ];
  ()

let test_proxy () =
  let ctx = ZMQ.init () in
  let pull_endpoint = "inproc://pull"
  and pub_endpoint = "inproc://pub"
  and pull = ZMQ.Socket.create ctx pull
  and pub = ZMQ.Socket.create ctx pub in

  let proxy (pull, pub) =
    ZMQ.Socket.bind pull pull_endpoint;
    ZMQ.Socket.bind pub pub_endpoint;
    (* Start the proxy and start relaying messages *)
    try
      ZMQ.Proxy.create pull pub;
      assert_failure "Proxy.create must raise an exception when completed"
    with
      ZMQ.ZMQ_exception _ -> ()
  in

  let _thread = Thread.create proxy (pull, pub) in
  sleep 10;
  let sub =
    let s = ZMQ.Socket.create ctx sub in
    ZMQ.Socket.connect s pub_endpoint;
    ZMQ.Socket.subscribe s "";
    s
  and push =
    let s = ZMQ.Socket.create ctx push in
    ZMQ.Socket.connect s pull_endpoint;
    s
  in
  let msg1 = "Message1"
  and msg2 = "Message2" in
  ZMQ.Socket.send push msg1;
  ZMQ.Socket.send push msg2;
  assert_equal msg1 (ZMQ.Socket.recv sub);
  assert_equal msg2 (ZMQ.Socket.recv sub);

  (** Epilog *)
  ZMQ.Socket.close sub;
  ZMQ.Socket.close push;
  ZMQ.Socket.close pull;
  ZMQ.Socket.close pub;
  ZMQ.term ctx;
  ()

let suite =
  "zmq test" >:::
    [
      "request reply" >::
        (bracket
           (fun () ->
             let ctx = init () in
             let req = create ctx req
             and rep = create ctx rep in
             ctx, req, rep
           )
           (fun (_, req, rep) ->
             let endpoint = "inproc://endpoint" in
             bind rep endpoint;
             connect req endpoint;
             send req "request";
             let msg = recv rep in
             assert_equal "request" msg;
             send rep "reply";
             let msg = recv req in
             assert_equal "reply" msg
           )
           (fun (ctx, req, rep) ->
             close req;
             close rep;
             term ctx
           ));

      "poll" >::
        (bracket
           (fun () ->
             let ctx = init () in
             let req = create ctx req
             and rep = create ctx rep
             and sub = create ctx sub in
             ctx, req, rep, sub
           )
           (fun (_, req, rep, sub) ->
             let endpoint = "inproc://endpoint" in

             bind rep endpoint;
             connect req endpoint;
             subscribe sub "";
             let mask = mask_of [| req, In_out; rep, In_out; sub, In_out |] in
             assert_equal [| Some Out; None; None |] (poll ~timeout:1000 mask);
             send req "request";
             assert_equal [| None; Some In; None |] (poll ~timeout:1000 mask);
             let msg = recv ~opt:R_no_block rep in
             assert_equal "request" msg;
             send rep "reply";
             assert_equal [| Some In; None; None |] (poll ~timeout:1000 mask);
             let msg = recv req in
             assert_equal "reply" msg;
           )
           (fun (ctx, req, rep, sub) ->
             close req;
             close rep;
             close sub;
             term ctx
           ));
      "get/set socket options" >:: test_options;
      "proxy" >:: test_proxy;
      "monitor" >:: test_monitor
    ]
