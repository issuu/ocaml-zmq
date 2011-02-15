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

let _ =
  Callback.register_exception "zmq exception" (ZMQ_exception(EUNKNOWN,"Unkown error"))
  
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

external version : unit -> int * int * int = "caml_zmq_version"

external init : int -> context = "caml_zmq_init"

external term : context -> unit = "caml_zmq_term"

external socket : context -> socket_kind -> socket = "caml_zmq_socket"

external close : socket -> unit = "caml_zmq_close"

external setsockopt : socket -> set_option -> unit = "caml_zmq_setsockopt"

external getsockoption : socket -> get_option_tag -> get_option = "caml_zmq_getsockoption"

external bind : socket -> string -> unit = "caml_zmq_bind"

external connect : socket -> string -> unit = "caml_zmq_connect"

external send : socket -> string -> send_recv_option -> unit = "caml_zmq_send"

external recv : socket -> send_recv_option -> string = "caml_zmq_recv"

