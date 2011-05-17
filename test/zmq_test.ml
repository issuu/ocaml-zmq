open OUnit

open ZMQ
open ZMQ.Socket
open ZMQ.Poll

let debug fmt = 
  Printf.ksprintf (fun s -> print_endline s; flush stdout) fmt 

let suite = 
  let setup () = init () 
  and teardown ctx = term ctx in
  let test f = (bracket setup f teardown) in
  "zmq test" >::: 
    [
      "request reply" >::
        (test
           (fun ctx -> 
             let endpoint = "inproc://endpoint"
             and req = create ctx req 
             and rep = create ctx rep in
             bind rep endpoint;
             connect req endpoint;
             send req "request";
             let msg = recv rep in
             assert_equal "request" msg;
             send rep "reply";
             let msg = recv req in
             assert_equal "reply" msg;
             close req;
             close rep
           ));

      "poll" >::
        (test
           (fun ctx -> 
             let endpoint = "inproc://endpoint"
             and req = create ctx req 
             and rep = create ctx rep in
             bind rep endpoint;
             connect req endpoint;
             let mask = mask_of [| req, In_out; rep, In_out |] in
             debug ">> polling";
             assert_equal [| None; None |] (poll ~timeout:1000 mask);
             debug ">> no data";
             send req "request";
             debug ">> sent request";
             assert_equal [| Some Out; Some In |] (poll ~timeout:1000 mask);
             debug ">> out data on req, in data on rep";
             let msg = recv ~opt:R_no_block rep in
             assert_equal "request" msg;
             send rep "reply";
             assert_equal [| None; Some In |] (poll ~timeout:1000 mask);
             let msg = recv req in
             assert_equal "reply" msg;
             close req;
             close rep
           ));
    ]
