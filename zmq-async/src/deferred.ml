open Async_kernel
open Async_unix

type 'a t = 'a Deferred.t
module Deferred = struct
  type 'a t = 'a Deferred.t
  let return a = Deferred.return a
  let try_with f = try_with ~extract_exn:true f
  let don't_wait_for f = don't_wait_for (f ())
  let fail exn = raise exn

  module Infix = struct
    let (>>=) = Deferred.(>>=)
  end
end

module Condition = struct
  type 'a deferred = 'a t
  type 'a t = 'a Ivar.t
  let create () = Ivar.create ()
  let wait t = Ivar.read t
  let wakeup t v = Ivar.fill t v
end

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
