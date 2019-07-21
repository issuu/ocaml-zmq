(* Copyright (c) 2011 Pedro Borges and contributors *)

(** Module Exceptions *)
type error =
| EFSM
| ENOCOMPATPROTO
| ETERM
| EMTHREAD
| EUNKNOWN

exception ZMQ_exception of error * string

val version : unit -> int * int * int

module Context : sig
  type t

  val create : unit -> t
  val terminate : t -> unit

  val get_io_threads : t -> int
  val set_io_threads : t -> int -> unit
  val get_max_sockets : t -> int
  val set_max_sockets : t -> int -> unit
  val get_ipv6 : t -> bool
  val set_ipv6 : t -> bool -> unit
end

module Msg : sig
  type t

  type bigstring =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  (** Initialize a new message with the given data.  The data will be
      kept alive for the lifetime of the message.

      @param offset specifies an offset from the start of the given block
             to use.  Defaults to [0].
      @param length specifies the number of bytes, starting from [offset],
             to use.  Defaults the [length of data - offset].
  *)
  val init_data : ?offset:int -> ?length:int -> bigstring -> t

  (** Size of the message in bytes *)
  val size : t -> int

  (** Retrieve a copy of the data contained in the message. *)
  val copy_data : t -> bigstring

  (** Retrieve the data contained in the message.

      This is considered {b unsafe} because the underlying data may be freed
      when the message's lifetime expires.
  *)
  val unsafe_data : t -> bigstring

  (** Free the message.  This will be done automatically when the message
      is garbage collected.
  *)
  val close : t -> unit

  (** Retrieve a property attached to a message. Property are simple strings.
      Example of properties include: "Socket-Type", "Identity", "Resource",
      but underlying transport and security mechanism may add more.
  *)
  val gets : t -> string -> string
end

module Socket : sig

  type 'a t
  type 'a kind

  val pair   : [>`Pair] kind
  val pub    : [>`Pub] kind
  val sub    : [>`Sub] kind
  val req    : [>`Req] kind
  val rep    : [>`Rep] kind
  val dealer : [>`Dealer] kind
  val router : [>`Router] kind
  val pull   : [>`Pull] kind
  val push   : [>`Push] kind
  val xsub   : [>`Xsub] kind
  val xpub   : [>`Xpub] kind
  val stream : [>`Stream] kind

  (** Creation and Destruction *)
  val create : Context.t -> 'a kind -> 'a t
  val close : 'a t -> unit

  (** Wiring *)
  val connect : 'a t -> string -> unit
  val disconnect : 'a t -> string -> unit
  val bind : 'a t -> string -> unit
  val unbind : 'a t -> string -> unit

  (** Read a message from the socket.
      block indicates if the call should be blocking or non-blocking.
      If block is [false], [recv] will raise [Unix.Unix_error (Unix.EAGAIN, _, _)] if there are no messages available to receive on the specified socket.
      Default true
  *)
  val recv : ?block:bool -> 'a t -> string

  (** Read a complete multipart message from the socket.
      block indicates if the call should be blocking or non-blocking. Default true
  *)
  val recv_all : ?block:bool -> 'a t -> string list

  (** Send a message to the socket.
      block indicates if the call should be blocking or non-blocking. Default true
      more is used for multipart messages, and indicates that the more message parts will follow. Default false
  *)
  val send : ?block:bool -> ?more:bool -> 'a t -> string -> unit

  (** Send a multipart message to the socket.
      block indicates if the call should be blocking or non-blocking. Default true
  *)
  val send_all : ?block:bool -> 'a t -> string list -> unit

  (** Receive a {!Msg.t} on the socket.

      @param block indicates if the call should be blocking or non-blocking.
             Defaults to [true].
  *)
  val recv_msg : ?block:bool -> 'a t -> Msg.t

  (** Receive a multi-part message on the socket.

      @param block indicates if the call should be blocking or non-blocking.
             Defaults to [true].
  *)
  val recv_msg_all : ?block:bool -> 'a t -> Msg.t list

  (** Send a {!Msg.t} to the socket.

      @param block indicates if the call should be blocking or non-blocking.
             Defaults to [true].
      @param more is used for multipart messages  Set to [true] to indicate that
             more message parts will follow.  Defaults to [false].
  *)
  val send_msg : ?block:bool -> ?more:bool -> 'a t -> Msg.t -> unit

  (** Send a multi-part message to the socket.

      @param block indicates if the call should be blocking or non-blocking.
             Defaults to [true].
  *)
  val send_msg_all : ?block:bool -> 'a t -> Msg.t list -> unit

  (** Option Getter and Setters *)
  val set_max_message_size : 'a t -> int -> unit
  val get_max_message_size : 'a t -> int
  val set_affinity : 'a t -> int -> unit
  val get_affinity : 'a t -> int
  val set_identity : 'a t -> string -> unit
  val get_identity : 'a t -> string
  val subscribe : [> `Sub] t -> string -> unit
  val unsubscribe : [> `Sub] t -> string -> unit
  val get_last_endpoint : 'a t -> string
  val set_tcp_accept_filter : 'a t -> string -> unit
  val set_rate : 'a t -> int -> unit
  val get_rate : 'a t -> int
  val set_recovery_interval : 'a t -> int -> unit
  val get_recovery_interval : 'a t -> int
  val set_send_buffer_size : 'a t -> int -> unit
  val get_send_buffer_size : 'a t -> int
  val set_receive_buffer_size : 'a t -> int -> unit
  val get_receive_buffer_size : 'a t -> int
  val has_more : 'a t -> bool
  val set_linger_period : 'a t -> int -> unit
  val get_linger_period : 'a t -> int
  val set_reconnect_interval : 'a t -> int -> unit
  val get_reconnect_interval : 'a t -> int
  val set_connection_backlog : 'a t -> int -> unit
  val get_connection_backlog : 'a t -> int
  val set_reconnect_interval_max : 'a t -> int -> unit
  val get_reconnect_interval_max : 'a t -> int
  val set_send_high_water_mark : 'a t -> int -> unit
  val get_send_high_water_mark : 'a t -> int
  val set_receive_high_water_mark : 'a t -> int -> unit
  val get_receive_high_water_mark : 'a t -> int
  val set_multicast_hops : 'a t -> int -> unit
  val get_multicast_hops : 'a t -> int
  val set_receive_timeout : 'a t -> int -> unit
  val get_receive_timeout : 'a t -> int
  val set_send_timeout : 'a t -> int -> unit
  val get_send_timeout : 'a t -> int
  val set_ipv6 : 'a t -> bool -> unit
  val get_ipv6 : 'a t -> bool
  val set_router_mandatory : 'a t -> bool -> unit
  val get_router_mandatory : 'a t -> bool
  val set_tcp_keepalive : 'a t -> [ `Default | `Value of bool ] -> unit
  val get_tcp_keepalive : 'a t -> [ `Default | `Value of bool ]
  val set_tcp_keepalive_idle : 'a t -> [ `Default | `Value of int ] -> unit
  val get_tcp_keepalive_idle : 'a t -> [ `Default | `Value of int ]
  val set_tcp_keepalive_count : 'a t -> [ `Default | `Value of int ] -> unit
  val get_tcp_keepalive_count : 'a t -> [ `Default | `Value of int ]
  val set_tcp_keepalive_interval : 'a t -> [ `Default | `Value of int ] -> unit
  val get_tcp_keepalive_interval : 'a t -> [ `Default | `Value of int ]
  val set_immediate : 'a t -> bool -> unit
  val get_immediate : 'a t -> bool
  val set_xpub_verbose : [> `XPub] t -> bool -> unit
  val set_probe_router : [> `Router | `Dealer | `Req ] t -> bool -> unit
  val set_req_correlate : [> `Req ] t -> bool -> unit
  val set_req_relaxed : [> `Req ] t -> bool -> unit
  val set_plain_server : 'a t -> bool -> unit
  val set_plain_username : 'a t -> string -> unit
  val get_plain_username : 'a t -> string
  val set_plain_password : 'a t -> string -> unit
  val get_plain_password : 'a t -> string
  val set_curve_server : 'a t -> bool -> unit
  val set_curve_publickey : 'a t -> string -> unit
  val get_curve_publickey : 'a t -> string
  val set_curve_secretkey : 'a t -> string -> unit
  val get_curve_secretkey : 'a t -> string
  val set_curve_serverkey : 'a t -> string -> unit
  val get_curve_serverkey : 'a t -> string
  val get_mechanism : 'a t -> [`Null | `Plain | `Curve]
  val set_zap_domain : 'a t -> string -> unit
  val get_zap_domain : 'a t -> string
  val set_conflate : [> `Pull | `Push | `Sub | `Pub | `Dealer] t -> bool -> unit

  val get_fd : 'a t -> Unix.file_descr

  type event = No_event | Poll_in | Poll_out | Poll_in_out | Poll_error
  val events : 'a t -> event

end

module Proxy : sig
  val create: ?capture:[> `Pub|`Dealer|`Push|`Pair] Socket.t -> 'a Socket.t -> 'b Socket.t -> unit
end

module Poll : sig

  type t

  type poll_event = In | Out | In_out
  type 'a poll_mask = ('a Socket.t * poll_event)

  val mask_of : 'a poll_mask array -> t
  val poll : ?timeout: int -> t -> poll_event option array

end

module Monitor : sig
  type t

  type address = string
  type error_no = int
  type error_text = string

  type event =
  | Connected of address * Unix.file_descr
  | Connect_delayed of address
  | Connect_retried of address * int (*interval*)
  | Listening of address * Unix.file_descr
  | Bind_failed of address * error_no * error_text
  | Accepted of address * Unix.file_descr
  | Accept_failed of address * error_no * error_text
  | Closed of address * Unix.file_descr
  | Close_failed of address * error_no * error_text
  | Disconnected of address * Unix.file_descr
  | Monitor_stopped of address
  | Handshake_failed_no_detail of address
  | Handshake_succeeded of address
  | Handshake_failed_protocol of address * int
  | Handshake_failed_auth of address * int


  val create: 'a Socket.t -> t
  val connect: Context.t -> t -> [>`Monitor] Socket.t

  (** Receive an event from the monitor socket.
      block indicates if the call should be blocking or non-blocking. Default true
  *)
  val recv: ?block:bool -> [> `Monitor ] Socket.t -> event

  val string_of_event: event -> string

  (** Create a memorizing function for converting an event to a string.
      As its it not possible to reliably retrieve the peer address of a closed socket
      dues to a race condition, this function pairs connects and disconnects and returns the matching
      connect peer address to disconnects.

      Note that it is not possible to retrieve the peer address of connect events is the peer has disconnected
      before string_of_event is called
  *)
  val mk_string_of_event: unit -> (event -> string)

end

module Z85 : sig
  val encode : string -> string
  val decode : string -> string
end

module Curve : sig
  (** [keypair ()] returns a pair [public, secret] of Z85 encoded keys. *)
  val keypair : unit -> string * string
end
