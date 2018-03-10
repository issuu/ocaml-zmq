module type T = sig
  type 'a t
  module Deferred : sig
    type nonrec 'a t = 'a t
    val return: 'a -> 'a t
    val try_with: (unit -> 'a t) -> ('a, exn) result t
    val don't_wait_for: (unit -> unit t) -> unit
    val fail: exn -> 'a t
    module Infix : sig
      val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    end
  end
  module Condition : sig
    type 'a t
    val create: unit -> 'a t
    val wait: 'a t -> 'a Deferred.t
    val wakeup: 'a t -> 'a -> unit
  end

  module Fd : sig
    type t
    (* Wrap the given unix file_deser *)
    val create: Unix.file_descr -> t

    (** Wait for the fd to become readable.
        It is important that all waiters on the socket are woken up,
        and not just one, as the fd is used as a broadcast mechanism.

        The Fd must _NOT_ be closed, as its owned by ZMQ.

    *)
    val wait_readable: t -> unit Deferred.t

  end
end
