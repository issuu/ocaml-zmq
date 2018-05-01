open OUnit

let suite = "ZMQ" >:::
  [
    Zmq_test.suite;
    Fd_usage.suite;
  ]

let _ =
  run_test_tt_main suite
