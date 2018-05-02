
(** This functor is meant to be as compatible as possible with lwt-zmq. It
    should be straight forward to write a functor over Async_zmq.Socket and
    Lwt_zmq.Socket.

    The functor allows abstraction of the concurrency monad
*)
module Make : functor (T : Deferred.T) -> sig
  open T

  (** An concurrent zeromq socket *)
  type 'a t

  (** [of_socket s] wraps the zeromq socket [s]*)
  val of_socket : 'a Zmq.Socket.t -> 'a t

  (** [to_socket s] extracts the raw zeromq socket from [s] *)
  val to_socket : 'a t -> 'a Zmq.Socket.t

  (** Internal statisitcs of the socket *)
  val to_string_hum : 'a t -> string

  (** [recv socket] waits for a message on [socket] without blocking
      other concurrent threads *)
  val recv : 'a t -> string Deferred.t

  (** [send socket] sends a message on [socket] without blocking other
      concurrent threads *)
  val send : 'a t -> string -> unit Deferred.t

  (** [recv_all socket] waits for a multi-part message on [socket] without
      blocking other concurrent threads *)
  val recv_all : 'a t -> string list Deferred.t

  (** [send_all socket m] sends all parts of the multi-part message [m] on
      [socket] without blocking other concurrent threads *)
  val send_all : 'a t -> string list -> unit Deferred.t

  (** [recv_msg socket] waits for a message on [socket] without blocking
      other concurrent threads *)
  val recv_msg : 'a t -> Zmq.Msg.t Deferred.t

  (** [send_msg socket] sends a message on [socket] without blocking other
      concurrent threads *)
  val send_msg : 'a t -> Zmq.Msg.t -> unit Deferred.t

  (** [recv_msg_all socket] waits for a multi-part message on [socket] without
      blocking other concurrent threads *)
  val recv_msg_all : 'a t -> Zmq.Msg.t list Deferred.t

  (** [send_msg_all socket m] sends all parts of the multi-part message [m] on
      [socket] without blocking other concurrent threads *)
  val send_msg_all : 'a t -> Zmq.Msg.t list -> unit Deferred.t

  val close : 'a t -> unit Deferred.t


  module Router : sig

    (** Identity of a socket connected to the router. *)
    type id_t

    (** [id_of_string s] coerces [s] into an {!id_t}. *)
    val id_of_string : string -> id_t

    (** [recv socket] waits for a message on [socket] without blocking other Lwt
        threads. *)
    val recv : [ `Router ] t -> (id_t * string list) Deferred.t

    (** [send socket id message] sends [message] on [socket] to [id] without
        blocking other Lwt threads. *)
    val send : [ `Router ] t -> id_t -> string list -> unit Deferred.t
  end

  module Monitor : sig
    (** [recv socket] waits for a monitoring event on [socket] without blocking other concurrent threads. *)
    val recv : [ `Monitor ] t -> Zmq.Monitor.event Deferred.t
  end

end
