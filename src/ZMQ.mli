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

module Socket :
sig
  type +'a t
  type 'a kind

  type generic
  type pair   = private generic
  type pub    = private generic
  type sub    = private generic
  type req    = private generic
  type rep    = private generic
  type dealer = private generic
  type router = private generic
  type pull   = private generic
  type push   = private generic

  val pair   : pair kind
  val pub    : pub kind
  val sub    : sub kind
  val req    : req kind
  val rep    : rep kind
  val dealer : dealer kind
  val router : router kind
  val pull   : pull kind
  val push   : push kind

  (** Creation and Destruction *)
  val create : context -> 'a kind -> 'a t
  val close : 'a t -> unit

  (** Wiring *)
  val connect : 'a t -> string -> unit
  val bind : 'a t -> string -> unit

  (** Send and Receive *)
  type recv_option = R_none | R_no_block
  val recv : ?opt:recv_option -> 'a t -> string

  type snd_option = S_none | S_no_block | S_more
  val send : ?opt:snd_option -> 'a t -> string -> unit

  (** Option Setters *)
  exception Invalid_identity of string
  val set_indentity : 'a t -> string -> unit
  val set_high_water_mark : 'a t -> Uint64.t -> unit
  val set_swap : 'a t -> int64 -> unit
  val set_affinity : 'a t -> Uint64.t -> unit
  val set_rate : 'a t -> int64 -> unit
  val set_recovery_interval : 'a t -> int64 -> unit
  val set_recovery_interval_msec : 'a t -> int64 -> unit
  val set_multicast_loop : 'a t -> bool -> unit
  val set_recv_buffer_size : 'a t -> Uint64.t -> unit
  val set_snd_buffer_size : 'a t -> Uint64.t -> unit
  val set_linger : 'a t -> int -> unit
  val set_reconnect_interval : 'a t -> int -> unit
  val set_reconnect_interval_max : 'a t -> int -> unit
  val set_backlog : 'a t -> int -> unit

  val subscribe : sub t -> string -> unit
  val unsubscribe : sub t -> string -> unit

  (** Option Getters *)
  val has_more : 'a t -> bool
  val high_water_mark : 'a t -> Uint64.t
  val swap : 'a t -> int64
  val affinity : 'a t -> Uint64.t
  val identity : 'a t -> string
  val rate : 'a t -> int64
  val recovery_interval : 'a t -> int64
  val recovery_interval_msec : 'a t -> int64
  val multicast_loop : 'a t -> int64
  val snd_buffer_size : 'a t -> Uint64.t
  val recv_buffer_size : 'a t -> Uint64.t
  val linger : 'a t -> int
  val reconnect_interval : 'a t -> int
  val reconnect_interval_max : 'a t -> int
  val backlog : 'a t -> int
  val get_fd : 'a t -> Unix.file_descr

  type event = No_event | Poll_in | Poll_out | Poll_in_out
  val events : 'a t -> event

  (** val kind: 'a t -> ? *)
  (** val fd : 'a t -> int ? incompatible with windows *)

end


module Device :
sig

  val streamer  :   Socket.pull Socket.t ->   Socket.push Socket.t -> unit
  val forwarder :    Socket.sub Socket.t ->    Socket.pub Socket.t -> unit
  val queue     : Socket.router Socket.t -> Socket.dealer Socket.t -> unit

end

module Poll :
sig

  type t

  type event_mask = In | Out | In_out
  type 'a poll_item = ('a Socket.t * event_mask)

  val of_poll_items : 'a poll_item array -> t

  val poll : ?timeout: int -> t -> 'a poll_item array

end
