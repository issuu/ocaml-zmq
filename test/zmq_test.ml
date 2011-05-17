open ZMQ
open ZMQ.Socket
open ZMQ.Poll

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
    ]
