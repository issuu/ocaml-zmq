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

  let hmw = 1324 in
  set_recevice_high_water_mark socket hmw;
  assert_equal ~msg:"Hightwater mark not set correctly" hmw (get_recevice_high_water_mark socket);
  let hmw = 1325 in
  set_send_high_water_mark socket hmw;
  assert_equal ~msg:"Hightwater mark not set correctly" hmw (get_send_high_water_mark socket);
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
      assert_failure "Exception should be raised"
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
  (* sleep 100; *)

  let msg1 = "Message1"
  and msg2 = "Message2" in
  ZMQ.Socket.send push msg1;
  ZMQ.Socket.send push msg2;
  assert_equal msg1 (ZMQ.Socket.recv sub);
  assert_equal msg2 (ZMQ.Socket.recv sub);

  (** Prolog *)
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
             and rep = create ctx rep in
             ctx, req, rep
           )
           (fun (_, req, rep) ->
             let endpoint = "inproc://endpoint" in
             bind rep endpoint;
             connect req endpoint;
             let mask = mask_of [| req, In_out; rep, In_out |] in
             assert_equal [| Some Out; None |] (poll ~timeout:1000 mask);
             send req "request";
             assert_equal [| None; Some In |] (poll ~timeout:1000 mask);
             let msg = recv ~opt:R_no_block rep in
             assert_equal "request" msg;
             send rep "reply";
             assert_equal [| Some In; None |] (poll ~timeout:1000 mask);
             let msg = recv req in
             assert_equal "reply" msg;
           )
           (fun (ctx, req, rep) ->
             close req;
             close rep;
             term ctx
           ));
      "get/set socket options" >:: test_options;
      "proxy" >:: test_proxy
    ]
