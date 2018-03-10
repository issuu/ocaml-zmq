open Lwt

type 'a t = 'a Lwt.t
let (>>=) = Lwt.(>>=)
let return a = Lwt.return a
let try_with f = Lwt_result.catch (f ())

module Fd = struct
  type nonrec 'a t' = 'a t
  type t = Lwt_unix.file_descr

  let create fd = Lwt_unix.of_unix_file_descr fd
  let wait_readable t = Lwt_unix.wait_read t

  let release _t = ()
end
