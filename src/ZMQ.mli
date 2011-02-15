(* Copyright (c) 2011 Pedro Borges and contributors *)

type context
type socket

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

type socket_kind = 
    Pair
  | Pub
  | Sub
  | Req
  | Rep
  | XReq
  | XRep
  | Pull
  | Push

type set_get_option = 
  [
    `High_water_mark of Uint64.t
  | `Swap of int64
  | `Affinity of Uint64.t
  | `Identity of string
	| `Rate of int64 
  | `Recovery_interval of int64
  | `Multicast_loop of int64
  | `Send_buffer of Uint64.t
  | `Recieve_buffer of Uint64.t
  ]

type set_option =
  [ set_get_option | `Subscribe of string | `Unsubscribe of string ]

type get_option =
  [ set_get_option | `Recieve_more of int64 ]

type get_option_tag =
  [
    `High_water_mark
  | `Swap
  | `Affinity
  | `Identity
  | `Rate
  | `Recovery_interval
  | `Multicast_loop
  | `Send_buffer
  | `Recieve_buffer
  | `Recieve_more
  ]

type send_recv_option =
  None | No_block | Snd_more

val version : unit -> int * int * int

val init : int -> context
val term : context -> unit

val socket : context -> socket_kind -> socket
val close : socket -> unit

val setsockopt : socket -> set_option -> unit
val getsockoption : socket -> get_option_tag -> get_option
val bind : socket -> string -> unit
val connect : socket -> string -> unit

val send : socket -> string -> send_recv_option -> unit
val recv : socket -> send_recv_option -> string

