module type T = sig
  type 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val return: 'a -> 'a t
  val try_with: (unit -> 'a t) -> ('a, exn) result t

  module Fd : sig
    type 'a t' = 'a t
    type t
    val syscall_exn : t -> (unit -> 'a) -> 'a t'
    val create : Unix.file_descr -> t
    val ready_to : t -> [ `Read | `Write ] -> [ `Bad_fd | `Closed | `Ready ] t'
    val close : t -> unit t'
  end
end
