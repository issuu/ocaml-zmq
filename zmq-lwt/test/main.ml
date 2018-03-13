module Test = Zmq_deferred_test.Test.Make(Deferred)

let () = Test.run (fun f -> Lwt_main.run (f ()))
