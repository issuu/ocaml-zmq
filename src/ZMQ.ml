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

let _ =
  Callback.register_exception "zmq exception" (ZMQ_exception(EUNKNOWN,"Unkown error"))


(** Context *)
type context

(** Creation and Destruction *)

external native_init : int -> context = "caml_zmq_init"
let init ?(io_threads = 1) () = native_init io_threads

external term : context -> unit = "caml_zmq_term"

external version : unit -> int * int * int = "caml_zmq_version"

module Socket = struct

  type 'a t

  (** This is an int so we know which socket we
    * are building inside the external functions *)

  type 'a kind = int

  type pair
  type pub
  type sub
  type req
  type rep
  type xreq
  type xrep
  type pull
  type push

  let pair = 0
  let pub  = 1
  let sub  = 2
  let req  = 3
  let rep  = 4
  let xreq = 5
  let xrep = 6
  let pull = 7
  let push = 8

  (** Creation and Destruction *)
  external create : context -> 'a kind -> 'a t = "caml_zmq_socket"
  external close : 'a t -> unit = "caml_zmq_close"

  (** Wiring *)
  external connect : 'a t -> string -> unit = "caml_zmq_connect"
  external bind : 'a t -> string -> unit = "caml_zmq_bind"

  (** Send and Receive *)
  type recv_option = R_none | R_no_block

  external native_recv : 'a t -> recv_option -> string = "caml_zmq_recv"
  let recv ?(opt = R_none) socket = native_recv socket opt
  
  type snd_option = S_none | S_no_block | S_more

  external native_send : 'a t -> string -> snd_option-> unit = "caml_zmq_send"
  let send ?(opt = S_none) socket message = native_send socket message opt


  (** Native Option Setters (private) *)
  type int64_option =
      Swap
    | Rate
    | Recovery_interval
    | Multicast_loop
    | Receive_more

  external set_int64_option :
    'a t -> int64_option -> int64 -> unit = "caml_zmq_set_int64_option"  
  
  type bytes_option =
      Identity
    | Subscribe
    | Unsubscribe

  external set_bytes_option :
    'a t -> bytes_option -> string -> unit = "caml_zmq_set_bytes_option"  

  type uint64_option = 
      High_water_mark
    | Affinity
    | Send_buffer
    | Receive_buffer

  external set_uint64_option :
    'a t -> uint64_option -> Uint64.t -> unit = "caml_zmq_set_uint64_option"  

  (** Option Setters *)
  let set_high_water_mark socket new_mark =
    set_uint64_option socket High_water_mark new_mark

  let set_swap socket new_swap =
    set_int64_option socket Swap new_swap

  let set_affinity socket new_affinity =
    set_uint64_option socket Affinity new_affinity

  exception Invalid_identity of string

  let identity_max_len = 255 and identity_min_len = 1

  let set_indentity socket new_identity =
    let identity_len = String.length new_identity in
      if identity_len < identity_min_len || identity_len > identity_max_len then
        raise (Invalid_identity new_identity)
      else
        set_bytes_option socket Identity new_identity

  let subscribe socket new_subscription =
    set_bytes_option socket Subscribe new_subscription

  let unsubscribe socket old_subscription =
    set_bytes_option socket Unsubscribe old_subscription
  
  let set_rate socket new_rate =
    set_int64_option socket Rate new_rate

  let set_recovery_interval socket new_rinterval =
    set_int64_option socket Recovery_interval new_rinterval

  let set_multicast_loop socket new_mcast_loop =
    let int64_val = if new_mcast_loop then 1L else 0L in
      set_int64_option socket Multicast_loop int64_val

  let set_recv_buffer_size socket new_size =
    set_uint64_option socket Receive_buffer new_size

  let set_snd_buffer_size socket new_size =
    set_uint64_option socket Send_buffer new_size


  (** Native Option Getters (private) *)
  external get_int64_option :
    'a t -> int64_option -> int64 = "caml_zmq_get_int64_option"

  external get_bytes_option :
    'a t -> bytes_option -> string = "caml_zmq_get_bytes_option"

  external get_uint64_option :
    'a t -> uint64_option -> Uint64.t = "caml_zmq_get_uint64_option"

  (** Option Getters *)
  let has_more socket =
    let opt_value = get_int64_option socket Receive_more in
      opt_value = 1L

  let high_water_mark socket =
    get_uint64_option socket High_water_mark

  let swap socket =
    get_int64_option socket Swap

  let affinity socket =
    get_uint64_option socket Affinity

  let identity socket =
    get_bytes_option socket Identity

  let rate socket =
    get_int64_option socket Rate

  let recovery_interval socket =
    get_int64_option socket Recovery_interval

  let multicast_loop socket =
    get_int64_option socket Multicast_loop

  let snd_buffer_size socket =
    get_uint64_option socket Send_buffer

  let recv_buffer_size socket =
    get_uint64_option socket Receive_buffer

end

module Device = struct

  type kind = 
      Streamer
    | Forwarder
    | Queue

  external create :
    kind -> 'a Socket.t -> 'b Socket.t -> unit = "caml_zmq_device"
 
  let streamer frontend backend = create Streamer frontend backend
  let forwarder frontend backend = create Forwarder frontend backend
  let queue frontend backend = create Queue frontend backend

end
