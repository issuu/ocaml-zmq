type 'a t = 'a Lwt.t
module Deferred = struct
  type 'a t = 'a Lwt.t
  let return a = Lwt.return a
  let catch f = Lwt_result.catch (f ())
  let don't_wait_for = Lwt.async
  let sleepf secs = Lwt_unix.sleep secs
  let fail exn = Lwt.fail exn

  module Infix = struct
    let (>>=) = Lwt.(>>=)
  end
end

module Condition = struct
  type 'a t = ('a Lwt.t * 'a Lwt.u)
  let create () = Lwt.wait ()
  let wait (t, _) = t
  let wakeup (_, u) v =
    Lwt.async (fun () -> Lwt.wakeup u v; Lwt.return ())
end

module Fd = struct
  type t = Lwt_unix.file_descr

  let create fd = Lwt_unix.of_unix_file_descr fd
  let wait_readable t = Lwt_unix.wait_read t
  let release _ = Deferred.return ()
end
