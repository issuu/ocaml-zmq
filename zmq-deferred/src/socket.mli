(** The functor allows abstraction of the concurrency monad *)

module type Socket = sig
  type 'a deferred

  (** An concurrent zeromq socket *)
  type 'a t

  type 'a of_socket_args

  (** [of_socket s] wraps the zeromq socket [s]*)
  val of_socket : ('a Zmq.Socket.t -> 'a t) of_socket_args

  (** [to_socket s] extracts the raw zeromq socket from [s] *)
  val to_socket : 'a t -> 'a Zmq.Socket.t

  (** [recv socket] waits for a message on [socket] without blocking
      other concurrent threads *)
  val recv : 'a t -> string deferred

  (** [send socket] sends a message on [socket] without blocking other
      concurrent threads *)
  val send : 'a t -> string -> unit deferred

  (** [recv_all socket] waits for a multi-part message on [socket] without
      blocking other concurrent threads *)
  val recv_all : 'a t -> string list deferred

  (** [send_all socket m] sends all parts of the multi-part message [m] on
      [socket] without blocking other concurrent threads *)
  val send_all : 'a t -> string list -> unit deferred

  (** [recv_msg socket] waits for a message on [socket] without blocking
      other concurrent threads *)
  val recv_msg : 'a t -> Zmq.Msg.t deferred

  (** [send_msg socket] sends a message on [socket] without blocking other
      concurrent threads *)
  val send_msg : 'a t -> Zmq.Msg.t -> unit deferred

  (** [recv_msg_all socket] waits for a multi-part message on [socket] without
      blocking other concurrent threads *)
  val recv_msg_all : 'a t -> Zmq.Msg.t list deferred

  (** [send_msg_all socket m] sends all parts of the multi-part message [m] on
      [socket] without blocking other concurrent threads *)
  val send_msg_all : 'a t -> Zmq.Msg.t list -> unit deferred

  val close : 'a t -> unit deferred


  module Router : sig

    (** Identity of a socket connected to the router. *)
    type id_t

    (** [id_of_string s] coerces [s] into an {!id_t}. *)
    val id_of_string : string -> id_t

    (** [recv socket] waits for a message on [socket] without blocking other Lwt
        threads. *)
    val recv : [ `Router ] t -> (id_t * string list) deferred

    (** [send socket id message] sends [message] on [socket] to [id] without
        blocking other Lwt threads. *)
    val send : [ `Router ] t -> id_t -> string list -> unit deferred
  end

  module Monitor : sig
    (** [recv socket] waits for a monitoring event on [socket] without blocking other concurrent threads. *)
    val recv : [ `Monitor ] t -> Zmq.Monitor.event deferred
  end

end

module Make : functor (T : Deferred.T) -> Socket with type 'a deferred = 'a T.t and type 'a of_socket_args = 'a
