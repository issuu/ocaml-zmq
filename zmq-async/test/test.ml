module Test = Zmq_deferred_test.Test.Make(Zmq_async__Deferred)

let () = Test.run Async_unix.Thread_safe.block_on_async_exn
