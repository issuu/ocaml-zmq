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
  type 'a t
  type 'a kind

  type pair
  type pub
  type sub
  type req
  type rep
  type xreq
  type xrep
  type pull
  type push

  val pair : pair kind
  val pub  : pub kind
  val sub  : sub kind
  val req  : req kind
  val rep  : rep kind
  val xreq : xreq kind
  val xrep : xrep kind
  val pull : pull kind
  val push : push kind

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
  val set_multicast_loop : 'a t -> bool -> unit
  val set_recv_buffer_size : 'a t -> Uint64.t -> unit
  val set_snd_buffer_size : 'a t -> Uint64.t -> unit

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
  val multicast_loop : 'a t -> int64
  val snd_buffer_size : 'a t -> Uint64.t
  val recv_buffer_size : 'a t -> Uint64.t
end


module Device :
sig

  type kind = 
      Streamer
    | Forwarder
    | Queue

  val create: kind -> 'a Socket.t -> 'a Socket.t -> unit
  
end
