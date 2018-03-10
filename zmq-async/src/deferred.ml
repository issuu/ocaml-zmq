open Async_kernel
open Async_unix

type 'a t = 'a Deferred.t
let (>>=) = Deferred.(>>=)
let return a = Deferred.return a
let try_with f = try_with ~extract_exn:true f

module Fd = struct
  type 'a t' = 'a t
  type t = Fd.t
  let create fd =
    Fd.create (Fd.Kind.Socket `Active) fd (Base.Info.of_string "<zmq>")

  let wait_readable: t -> unit t' = fun t ->
    Fd.ready_to t `Read >>= function
    | `Bad_fd -> failwith "Bad filedescriptor"
    | `Closed -> failwith "Filedescr closed unexpectedly"
    | `Ready -> return ()

  let release _ = ()
end
