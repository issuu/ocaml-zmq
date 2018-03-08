open Lwt

type 'a t = 'a Lwt.t
let (>>=) = Lwt.(>>=)
let return a = Lwt.return a
let try_with f = Lwt_result.catch (f ())

module Fd = struct
  type nonrec 'a t' = 'a t
  type t = Lwt_unix.file_descr

  let create fd = Lwt_unix.of_unix_file_descr fd
  let syscall_exn fd f = Lwt_unix.wrap_syscall Lwt_unix.Read fd f
  let ready_to t kind =
    let f = match kind with
      | `Read -> Lwt_unix.wait_read
      | `Write -> Lwt_unix.wait_write
    in
    Lwt.catch
        (fun () -> f t >|= fun () -> `Ready)
        (fun _exn -> return `Closed)

  let close t = Lwt_unix.close t
end
