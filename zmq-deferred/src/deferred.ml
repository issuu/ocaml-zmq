module type T = sig
  type 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val return: 'a -> 'a t
  val try_with: (unit -> 'a t) -> ('a, exn) result t

  module Fd : sig
    type 'a t' = 'a t
    type t
    (* Wrap the given unix file_deser *)
    val create: Unix.file_descr -> t

    (** Wait for the fd to become readable.
        It is important that all waiters on the socket are woken up,
        and not just one, as the fd is used as a broadcast mechanism
    *)
    val wait_readable: t -> unit t'

    (** Release resources acquired for this fd
        The Fd must _NOT_ be closed, as its owned by ZMQ.

        "The returned file descriptor is intended for use with a poll
        or similar system call only. Applications must never attempt to
        read or write data to it directly, neither should they try to close
        it." (http://api.zeromq.org/master:zmq-getsockopt#ZMQ_FD)
    *)
    val release: t -> unit
  end
end
