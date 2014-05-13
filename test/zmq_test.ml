open OUnit

open ZMQ
open ZMQ.Context
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

let test_ctx_options () =
  let ctx = Context.create () in

  let test_set_get_int msg setter getter ctx v =
    let default = getter ctx in
    setter ctx v;
    assert_equal ~msg ~printer:string_of_int v (getter ctx);
    setter ctx default;
    assert_equal ~msg default (getter ctx);
    ()
  in

  let test_set_get_bool msg setter getter ctx v =
    let default = getter ctx in
    setter ctx v;
    assert_equal ~msg ~printer:string_of_bool v (getter ctx);
    setter ctx default;
    assert_equal ~msg default (getter ctx);
    ()
  in

  test_set_get_int "IO threads" set_io_threads get_io_threads ctx 1;
  test_set_get_int "Max sockets" set_max_sockets get_max_sockets ctx 1024;
  test_set_get_bool "IPv6" Context.set_ipv6 Context.get_ipv6 ctx false;
  ()

let test_options () =
  let socket =
    let ctx = Context.create () in
    let s = create ctx push in
    s
  in

  let test_set_get_int msg setter getter socket v =
    let default = getter socket in
    setter socket v;
    assert_equal ~msg ~printer:string_of_int v (getter socket);
    setter socket default;
    assert_equal ~msg default (getter socket);
    ()
  in

  let test_set_get_value msg setter getter socket v =
    let default = getter socket in
    setter socket v;
    assert_equal ~msg ~printer:(function `Default -> "default" | `Value n -> string_of_int n) v (getter socket);
    setter socket default;
    assert_equal ~msg default (getter socket);
    ()
  in


  test_set_get_int "Highwatermark" set_receive_high_water_mark get_receive_high_water_mark socket 1235;
  test_set_get_int "Affinity" set_affinity get_affinity socket 3;
  test_set_get_int "Receive timeout" set_receive_timeout get_receive_timeout socket 1000;
  test_set_get_value "Tcp keepalive interval" set_tcp_keepalive_interval get_tcp_keepalive_interval socket (`Value 1000);
  ()

let test_monitor () =
  let ctx = Context.create () in
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
        (ZMQ.Monitor.string_of_event (ZMQ.Monitor.recv ~block:false socket));
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
  let ctx = ZMQ.Context.create () in
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
      Unix.Unix_error (Unix.ENOTSOCK, _, _) -> ()
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
  ZMQ.Context.terminate ctx;
  ()

(** Simple test to test interrupted exception, while in the C lib. *)
let test_unix_exceptions = bracket
    (fun () ->
       let ctx = ZMQ.Context.create () in
       let s = ZMQ.Socket.create ctx pull in
       (ctx, s)
    )
    (fun (_, s) ->

       let mask = ZMQ.Poll.mask_of [| s, ZMQ.Poll.In |] in
       Sys.(set_signal sigalrm (Signal_handle (fun _ -> ())));
       ignore (Unix.alarm 1);
       assert_raises ~msg:"Failed to raise EINTR" Unix.(Unix_error(EINTR, "zmq_poll", ""))  (fun _ -> ZMQ.Poll.poll ~timeout:2000 mask);
       ()
    )
    (fun (ctx, s) ->
       ZMQ.Socket.close s;
       ZMQ.Context.terminate ctx
    )

(** Test a ZMQ specific exception *)
let test_zmq_exception = bracket
  (fun () ->
    let ctx = ZMQ.Context.create () in
    let socket = ZMQ.Socket.create ctx req in
    (ctx, socket)
  )
  (fun (_, socket) ->
     assert_raises
       (ZMQ.ZMQ_exception(ZMQ.EFSM, "Operation cannot be accomplished in current state"))
       (fun () -> ZMQ.Socket.recv socket);
  )
  (fun (ctx, socket) ->
     ZMQ.Socket.close socket;
     ZMQ.Context.terminate ctx;
  )

let test_z85 () =
  let binary = "\xBB\x88\x47\x1D\x65\xE2\x65\x9B" ^
               "\x30\xC5\x5A\x53\x21\xCE\xBB\x5A" ^
               "\xAB\x2B\x70\xA3\x98\x64\x5C\x26" ^
               "\xDC\xA2\xB2\xFC\xB4\x3F\xC5\x18" in
  let ascii  = "Yne@$w-vo<fVvi]a<NY6T1ed:M$fCG*[IaLV{hID" in
  assert_equal ~printer:(Printf.sprintf "%S") binary (ZMQ.Z85.decode ascii);
  assert_equal ~printer:(Printf.sprintf "%S") ascii  (ZMQ.Z85.encode binary);

  assert_raises (Invalid_argument "zmq_z85_encode") (fun () -> ZMQ.Z85.encode "123");
  assert_raises (Invalid_argument "zmq_z85_decode") (fun () -> ZMQ.Z85.decode "123");
  ()

let suite =
  "zmq test" >:::
    [
      "request reply" >::
        (bracket
           (fun () ->
             let ctx = ZMQ.Context.create () in
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
             ZMQ.Context.terminate ctx
           ));

      "request reply (multi-part)" >::
        (bracket
           (fun () ->
             let ctx = ZMQ.Context.create () in
             let req = create ctx req
             and rep = create ctx rep in
             ctx, req, rep
           )
           (fun (_, req, rep) ->
             let endpoint = "inproc://endpoint" in
             bind rep endpoint;
             connect req endpoint;
             send_all req ["request"; "and more"];
             let msg = recv_all rep in
             assert_equal ["request"; "and more"] msg;
             send_all rep ["reply"; "and more"];
             let msg = recv_all req in
             assert_equal ["reply"; "and more"] msg
           )
           (fun (ctx, req, rep) ->
             close req;
             close rep;
             ZMQ.Context.terminate ctx
           ));

      "poll" >::
        (bracket
           (fun () ->
             let ctx = ZMQ.Context.create () in
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
             let msg = recv ~block:false rep in
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
             ZMQ.Context.terminate ctx
           ));
      "get/set context options" >:: test_ctx_options;
      "get/set socket options" >:: test_options;
      "proxy" >:: test_proxy;
      "monitor" >:: test_monitor;
      "z85 encoding/decoding" >:: test_z85;
      "unix exceptions" >:: test_unix_exceptions;
      "zmq exceptions" >:: test_zmq_exception;
    ]
