open Async_kernel
open Async_unix

type 'a t = 'a Deferred.t
let (>>=) = Deferred.(>>=)
let return a = Deferred.return a
let try_with f = try_with ~extract_exn:true f

module Fd = struct
  type 'a t' = 'a t
  type t = Fd.t
  let syscall_exn _fd f = In_thread.syscall_exn ~name:"<zmq>" f
  let create fd =
    Fd.create (Fd.Kind.Socket `Active) fd (Base.Info.of_string "<zmq>")
  let ready_to t kind = Fd.ready_to t kind
  let close t = Fd.close t
end
