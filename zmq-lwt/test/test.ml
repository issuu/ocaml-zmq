module Test = Zmq_deferred_test.Test.Make(Zmq_lwt__Deferred)
let () = Test.run (fun f -> Lwt_main.run (f ()))
