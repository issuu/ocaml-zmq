module Socket : sig

  (** An Lwt-wrapped zeromq socket *)
  type 'a t

  (** [of_socket s] wraps the zeromq socket [s] for use with Lwt *)
  val of_socket : 'a ZMQ.Socket.t -> 'a t

  (** [to_socket s] extracts the raw zeromq socket from [s] *)
  val to_socket : 'a t -> 'a ZMQ.Socket.t

  (** [recv socket] waits for a message on [socket] without blocking other Lwt
      threads *)
  val recv : 'a t -> string Lwt.t

  (** [send ?more socket m] sends a message [m] on [socket] without blocking other
      Lwt threads *)
  val send : ?more:bool -> 'a t -> string -> unit Lwt.t

  (** [recv_all socket] waits for a multi-part message on [socket] without
      blocking other Lwt threads *)
  val recv_all : 'a t -> string list Lwt.t

  (** [send_all socket m] sends all parts of the multi-part message [m] on
      [socket] without blocking other Lwt threads *)
  val send_all : 'a t -> string list -> unit Lwt.t

  module Router : sig

    (** Identity of a socket connected to the router. *)
    type id_t = private string

    (** [id_of_string s] coerces [s] into an {!id_t}. *)
    val id_of_string : string -> id_t

    (** [recv socket] waits for a message on [socket] without blocking other Lwt
        threads. *)
    val recv : [ `Router ] t -> (id_t * string list) Lwt.t

    (** [send socket id message] sends [message] on [socket] to [id] without
        blocking other Lwt threads. *)
    val send : [ `Router ] t -> id_t -> string list -> unit Lwt.t
  end
end

module Monitor : sig

  (** [recv socket] waits for a monitoring event on [socket] without blocking other
      Lwt threads. *)
  val recv : [ `Monitor ] Socket.t -> ZMQ.Monitor.event Lwt.t
end
