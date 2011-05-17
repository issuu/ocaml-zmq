open OUnit;;

let suite = "ZMQ" >::: 
  [
    zmq_test.suite;
  ]

let _ = 
  run_test_tt_main suite

