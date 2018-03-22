open Async_kernel
open Async_unix

type 'a t = 'a Deferred.t
module Deferred = struct
  type 'a t = 'a Deferred.t
  let return a = Deferred.return a
  let catch f = try_with ~extract_exn:true f
  let don't_wait_for f = don't_wait_for (f ())
  let sleepf secs = Async_kernel.after (Core.Time_ns.Span.of_sec secs)
  let fail exn = raise exn

  module Infix = struct
    let (>>=) = Deferred.(>>=)
    let (<?>) a b = Deferred.any [ a; b ]
  end
end

module Condition = struct
  type 'a t = 'a Condition.t
  let create () = Condition.create ()
  let wait t = Condition.wait t
  let signal t v = Condition.signal t v
end

module Mailbox = struct
  type 'a t = 'a Async_kernel.Ivar.t
  let create () = Async_kernel.Ivar.create ()
  let send t v = Async_kernel.Ivar.fill t v
  let recv t = Async_kernel.Ivar.read t
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

  let release t = Fd.close ~file_descriptor_handling:Fd.Do_not_close_file_descriptor t
end
