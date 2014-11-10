open OUnit;;

let suite = "ZMQ" >:::
  [
    Zmq_test.suite;
  ]

let _ =
  let _ = run_test_tt_main suite in
  let _ = Gc.compact () in
  ()
