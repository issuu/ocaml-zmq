open OUnit

open Zmq
open Zmq.Context
open Zmq.Socket
open Zmq.Poll

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
  Context.terminate ctx

let test_socket_options () =
  let ctx = Context.create () in
  let socket = create ctx push in


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

  Socket.close socket;
  Context.terminate ctx;
  ()

let test_monitor () =
  let endpoint = "ipc://monitor_socket" in
  let expect_handshake = match Zmq.version () with
    | (4, n, _) when n < 3 -> false
    | (3, _, _) -> failwith "Only zmq version 4 or higher is supported"
    | _ -> true
  in

  let assert_event name socket event =
    let printer x = x in
    let rec cmp str1 str2 =
      match String.length str1 <= String.length str2 with
      | true -> String.sub str2 0 (String.length str1) = str1
      | false -> cmp str2 str1
    in
    let rec receive_event ~end_time =
      match Zmq.Monitor.recv ~block:false socket with
      | event -> Zmq.Monitor.string_of_event event
      | exception Unix.Unix_error(Unix.EAGAIN, _, _) when end_time > (Unix.gettimeofday ()) ->
        sleep 10;
        receive_event ~end_time
      | exception Unix.Unix_error(Unix.EAGAIN, _, _) -> "No event received"
    in
    let received = receive_event ~end_time:(Unix.gettimeofday () +. 1.0) in
    assert_equal ~msg:(Printf.sprintf "Wrong event received on %s" name) ~printer ~cmp
      (Printf.sprintf "%s: %s" event endpoint) received
  in
  let ctx = Context.create () in
  let s1 = Zmq.Socket.create ctx Zmq.Socket.pair in
  let s2 = Zmq.Socket.create ctx Zmq.Socket.pair in
  let m1 =
    let mon_t = Zmq.Monitor.create s1 in
    Zmq.Monitor.connect ctx mon_t
  in
  let m2 =
    let mon_t = Zmq.Monitor.create s2 in
    Zmq.Monitor.connect ctx mon_t
  in
  Zmq.Socket.bind s1 endpoint;
  assert_event "m1" m1 "Listening";
  Zmq.Socket.connect s2 endpoint;
  assert_event "m1" m1 "Accepted";
  assert_event "m2" m2 "Connect";
  if (expect_handshake) then begin
    assert_event "m1" m1 "Handshake_succeeded";
    assert_event "m2" m2 "Handshake_succeeded"
  end;
  Zmq.Socket.close s1;
  assert_event "m1" m1 "Closed";
  Zmq.Socket.close s2;
  assert_event "m2" m2 "Disconnect";

  Zmq.Socket.close m2;
  Zmq.Socket.close m1;
  Context.terminate ctx;
  ()

let test_proxy () =
  let ctx = Zmq.Context.create () in
  let pull_endpoint = "inproc://pull" in
  let pub_endpoint = "inproc://pub" in
  let pull = Zmq.Socket.create ctx pull in
  let pub = Zmq.Socket.create ctx pub in

  let proxy (pull, pub) =
    Zmq.Socket.bind pull pull_endpoint;
    Zmq.Socket.bind pub pub_endpoint;
    (* Start the proxy and start relaying messages *)
    try
      Zmq.Proxy.create pull pub;
      assert_failure "Proxy.create must raise an exception when completed"
    with
      Unix.Unix_error (Unix.ENOTSOCK, _, _) -> ()
  in

  let _thread = Thread.create proxy (pull, pub) in
  sleep 10;
  let sub =
    let s = Zmq.Socket.create ctx sub in
    Zmq.Socket.connect s pub_endpoint;
    Zmq.Socket.subscribe s "";
    s
  in
  let push =
    let s = Zmq.Socket.create ctx push in
    Zmq.Socket.connect s pull_endpoint;
    s
  in
  let msg1 = "Message1" in
  let msg2 = "Message2" in
  Zmq.Socket.send push msg1;
  Zmq.Socket.send push msg2;
  assert_equal msg1 (Zmq.Socket.recv sub);
  assert_equal msg2 (Zmq.Socket.recv sub);

  (* Epilog *)
  Zmq.Socket.close sub;
  Zmq.Socket.close push;
  Zmq.Socket.close pull;
  Zmq.Socket.close pub;
  Zmq.Context.terminate ctx;
  ()

(** Simple test to test interrupted exception, while in the C lib. *)
let test_unix_exceptions = bracket
    (fun () ->
       let ctx = Zmq.Context.create () in
       let s = Zmq.Socket.create ctx pull in
       (ctx, s)
    )
    (fun (_, s) ->

       let mask = Zmq.Poll.mask_of [| s, Zmq.Poll.In |] in
       Sys.(set_signal sigalrm (Signal_handle (fun _ -> ())));
       ignore (Unix.alarm 1);
       assert_raises ~msg:"Failed to raise EINTR" Unix.(Unix_error(EINTR, "zmq_poll", ""))  (fun _ -> Zmq.Poll.poll ~timeout:2000 mask);
       ()
    )
    (fun (ctx, s) ->
       Zmq.Socket.close s;
       Zmq.Context.terminate ctx
    )

(** Test a Zmq specific exception *)
let test_zmq_exception = bracket
  (fun () ->
    let ctx = Zmq.Context.create () in
    let socket = Zmq.Socket.create ctx req in
    (ctx, socket)
  )
  (fun (_, socket) ->
     assert_raises
       (Zmq.ZMQ_exception(Zmq.EFSM, "Operation cannot be accomplished in current state"))
       (fun () -> Zmq.Socket.recv socket);
  )
  (fun (ctx, socket) ->
     Zmq.Socket.close socket;
     Zmq.Context.terminate ctx;
  )

let test_socket_gc () =
  let sock =
    let ctx = Zmq.Context.create () in
    Zmq.Socket.create ctx Zmq.Socket.req
  in
  (* This will hang trying to terminate the context if socket doesn't keep it alive *)
  Gc.compact ();
  Zmq.Socket.close sock;
  Gc.compact () (* Clean up the context.*)

let test_context_gc () =
  let ctx =
    let ctx = Zmq.Context.create () in
    let sock = Zmq.Socket.create ctx Zmq.Socket.pair in
    Zmq.Socket.connect sock "ipc://context_gc_socket";
    Zmq.Socket.send sock "test";
    ctx
  in
  (* At this point, ctx is alive. The garbage collector needs to set
     so_linger when closing the socket, or it the system will hang when
     trying to terminate the context. *)
  Gc.compact ();
  Zmq.Context.terminate ctx;
  ()

let test_z85 () =
  let binary = "\xBB\x88\x47\x1D\x65\xE2\x65\x9B" ^
               "\x30\xC5\x5A\x53\x21\xCE\xBB\x5A" ^
               "\xAB\x2B\x70\xA3\x98\x64\x5C\x26" ^
               "\xDC\xA2\xB2\xFC\xB4\x3F\xC5\x18" in
  let ascii  = "Yne@$w-vo<fVvi]a<NY6T1ed:M$fCG*[IaLV{hID" in
  assert_equal ~printer:(Printf.sprintf "%S") binary (Zmq.Z85.decode ascii);
  assert_equal ~printer:(Printf.sprintf "%S") ascii  (Zmq.Z85.encode binary);

  assert_raises (Invalid_argument "zmq_z85_encode") (fun () -> Zmq.Z85.encode "123");
  assert_raises (Invalid_argument "zmq_z85_decode") (fun () -> Zmq.Z85.decode "123");
  ()

let suite =
  "zmq test" >:::
    [
      "request reply" >::
        (bracket
           (fun () ->
             let ctx = Zmq.Context.create () in
             let req = create ctx req in
             let rep = create ctx rep in
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
             Zmq.Context.terminate ctx
           ));

      "request reply (multi-part)" >::
        (bracket
           (fun () ->
             let ctx = Zmq.Context.create () in
             let req = create ctx req in
             let rep = create ctx rep in
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
             Zmq.Context.terminate ctx
           ));

      "poll" >::
        (bracket
           (fun () ->
             let ctx = Zmq.Context.create () in
             let req = create ctx req in
             let rep = create ctx rep in
             let sub = create ctx sub in
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
             Zmq.Context.terminate ctx;
           ));
      "get/set context options" >:: test_ctx_options;
      "get/set socket options" >:: test_socket_options;
      "proxy" >:: test_proxy;
      "monitor" >:: test_monitor;
      "z85 encoding/decoding" >:: test_z85;
      "unix exceptions" >:: test_unix_exceptions;
      "zmq exceptions" >:: test_zmq_exception;
      (* Gc tests disabled, as resources will not be freed through finalisers
         "socket gc" >:: test_socket_gc;
         "context gc" >:: test_context_gc;
      *)
    ]
