open OUnit

let suite = "Zmq" >:::
  [
    Zmq_test.suite;
    Fd_usage.suite;
    Curve.suite;
  ]

let _ =
  run_test_tt_main suite
