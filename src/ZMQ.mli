(* Copyright (c) 2011 Pedro Borges and contributors *)

(** Module Exceptions *)
type error =
    EINVAL
  | EFAULT
  | EMTHREAD
  | ETERM
  | ENODEV
  | EADDRNOTAVAIL
  | EADDRINUSE
  | ENOCOMPATPROTO
  | EPROTONOSUPPORT
  | EAGAIN
  | ENOTSUP
  | EFSM
  | ENOMEM
  | EINTR
  | EUNKNOWN

exception ZMQ_exception of error * string

(** Context *)
type context

(** Creation and Destruction *)
val init : ?io_threads:int -> unit -> context
val term : context -> unit

val version : unit -> int * int * int

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

  (** Creation and Destruction *)
  val create : context -> 'a kind -> 'a t
  val close : 'a t -> unit

  (** Wiring *)
  val connect : 'a t -> string -> unit
  val bind : 'a t -> string -> unit
  val disconnect : 'a t -> string -> unit

  (** Send and Receive *)
  type recv_option = R_none | R_no_block
  val recv : ?opt:recv_option -> 'a t -> string

  type snd_option = S_none | S_no_block | S_more | S_more_no_block
  val send : ?opt:snd_option -> 'a t -> string -> unit

  (** Option Getter and Setters *)
  val set_max_message_size : 'a t -> int -> unit
  val get_max_message_size : 'a t -> int
  val set_affinity : 'a t -> int -> unit
  val get_affinity : 'a t -> int
  val set_identity : [> `Req | `Rep | `Router] t -> string -> unit
  val get_identity : [> `Req | `Rep | `Router] t -> string
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
  val set_ipv4_only : 'a t -> bool -> unit
  val get_ipv4_only : 'a t -> bool
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
  val set_delay_attach_on_connect : 'a t -> bool -> unit
  val get_delay_attach_on_connect : 'a t -> bool
  val set_xpub_verbose : [> `XPub] t -> bool -> unit

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
  | Connect_delayed of address * error_no * error_text
  | Connect_retried of address * int (*interval*)
  | Listening of address * Unix.file_descr
  | Bind_failed of address * error_no * error_text
  | Accepted of address * Unix.file_descr
  | Accept_failed of address * error_no * error_text
  | Closed of address * Unix.file_descr
  | Close_failed of address * error_no * error_text
  | Disconnected of address * Unix.file_descr

  val create: 'a Socket.t -> t
  val connect: context -> t -> [>`Monitor] Socket.t
  val recv: ?opt:Socket.recv_option -> [> `Monitor ] Socket.t -> event
  val string_of_event: event -> string

end
