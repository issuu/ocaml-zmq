open Core
open Async

(** This module is meant to be as compatible as possible with lwt-zmq. It
    should be straight forward to write a functor over Async_zmq.Socket and
    Lwt_zmq.Socket *)
module Socket : sig
  (** An Async-wrapped zeromq socket *)
  type 'a t [@@deriving sexp_of]

  (** [of_socket s] wraps the zeromq socket [s] for use with Async *)
  val of_socket : 'a ZMQ.Socket.t -> 'a t

  (** [to_socket s] extracts the raw zeromq socket from [s] *)
  val to_socket : 'a t -> 'a ZMQ.Socket.t

  (** [recv socket] waits for a message on [socket] without blocking
      other Async threads *)
  val recv : 'a t -> string Deferred.t

  (** [send socket] sends a message on [socket] without blocking other
      Async threads *)
  val send : 'a t -> string -> unit Deferred.t

  (** [recv_all socket] waits for a multi-part message on [socket] without
      blocking other Lwt threads *)
  val recv_all : 'a t -> string list Deferred.t

  (** [send_all socket m] sends all parts of the multi-part message [m] on
      [socket] without blocking other Lwt threads *)
  val send_all : 'a t -> string list -> unit Deferred.t

  val close : 'a t -> unit Deferred.t
end
