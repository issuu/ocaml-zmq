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
exception Illegal_argument

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

  let pair   = 0
  let pub    = 1
  let sub    = 2
  let req    = 3
  let rep    = 4
  let dealer = 5
  let router = 6
  let pull   = 7
  let push   = 8
  let xpub   = 9
  let xsub   = 10

  (** Creation and Destruction *)
  external create : context -> 'a kind -> 'a t = "caml_zmq_socket"
  external close : 'a t -> unit = "caml_zmq_close"

  (** Wiring *)
  external connect : 'a t -> string -> unit = "caml_zmq_connect"
  external disconnect : 'a t -> string -> unit = "caml_zmq_disconnect"
  external bind : 'a t -> string -> unit = "caml_zmq_bind"

  (** Send and Receive *)
  type recv_option = R_none | R_no_block

  external native_recv : 'a t -> recv_option -> string = "caml_zmq_recv"
  let recv ?(opt = R_none) socket = native_recv socket opt

  type snd_option = S_none | S_no_block | S_more | S_more_no_block

  external native_send : 'a t -> string -> snd_option -> unit = "caml_zmq_send"
  let send ?(opt = S_none) socket message = native_send socket message opt


  (** Native Option Setters (private) *)
  type int64_option =
  | ZMQ_MAXMSGSIZE

  external set_int64_option :
    'a t -> int64_option -> int64 -> unit = "caml_zmq_set_int64_option"

  external get_int64_option :
    'a t -> int64_option -> int64 = "caml_zmq_get_int64_option"


  type uint64_option =
  | ZMQ_AFFINITY

  external set_uint64_option :
    'a t -> uint64_option -> Uint64.t -> unit = "caml_zmq_set_uint64_option"

  external get_uint64_option :
    'a t -> uint64_option -> Uint64.t = "caml_zmq_get_uint64_option"


  type bytes_option =
  | ZMQ_IDENTITY
  | ZMQ_SUBSCRIBE
  | ZMQ_UNSUBSCRIBE
  | ZMQ_LAST_ENDPOINT
  | ZMQ_TCP_ACCEPT_FILTER

  external set_bytes_option :
    'a t -> bytes_option -> string -> unit = "caml_zmq_set_bytes_option"

  external get_bytes_option :
    'a t -> bytes_option -> string = "caml_zmq_get_bytes_option"

  type int_option =
  | ZMQ_RATE
  | ZMQ_RECOVERY_IVL
  | ZMQ_SNDBUF
  | ZMQ_RCVBUF
  | ZMQ_RCVMORE
  | ZMQ_EVENTS
  | ZMQ_TYPE
  | ZMQ_LINGER
  | ZMQ_RECONNECT_IVL
  | ZMQ_BACKLOG
  | ZMQ_RECONNECT_IVL_MAX
  | ZMQ_SNDHWM
  | ZMQ_RCVHWM
  | ZMQ_MULTICAST_HOPS
  | ZMQ_RCVTIMEO
  | ZMQ_SNDTIMEO
  | ZMQ_IPV4ONLY
  | ZMQ_ROUTER_MANDATORY
  | ZMQ_TCP_KEEPALIVE
  | ZMQ_TCP_KEEPALIVE_CNT
  | ZMQ_TCP_KEEPALIVE_IDLE
  | ZMQ_TCP_KEEPALIVE_INTVL
  | ZMQ_DELAY_ATTACH_ON_CONNECT
  | ZMQ_XPUB_VERBOSE

  external set_int_option :
    'a t -> int_option -> int -> unit = "caml_zmq_set_int_option"

  external get_int_option :
    'a t -> int_option -> int = "caml_zmq_get_int_option"


  let validate_string_length min max str =
    match String.length str with
    | n when n < min -> raise Illegal_argument
    | n when n > max -> raise Illegal_argument
    | n -> ()

  let set_max_message_size socket size =
    set_int64_option socket ZMQ_MAXMSGSIZE (Int64.of_int size)

  let get_max_message_size socket =
    Int64.to_int (get_int64_option socket ZMQ_MAXMSGSIZE)

  let set_affinity socket size =
    set_uint64_option socket ZMQ_AFFINITY (Uint64.of_int size)

  let get_affinity socket =
    Uint64.to_int (get_uint64_option socket ZMQ_AFFINITY)

  let set_identity socket identity =
    validate_string_length 1 255 identity;
    set_bytes_option socket ZMQ_IDENTITY identity

  let get_identity socket =
    get_bytes_option socket ZMQ_IDENTITY

  let subscribe socket topic =
    set_bytes_option socket ZMQ_SUBSCRIBE topic

  let unsubscribe socket topic =
    set_bytes_option socket ZMQ_UNSUBSCRIBE topic

  let get_last_endpoint socket =
    get_bytes_option socket ZMQ_LAST_ENDPOINT

  let set_tcp_accept_filter socket filter =
    set_bytes_option socket ZMQ_TCP_ACCEPT_FILTER filter

  let set_rate socket rate =
    set_int_option socket ZMQ_RATE rate

  let get_rate socket =
    get_int_option socket ZMQ_RATE

  let set_recovery_interval socket interval =
    set_int_option socket ZMQ_RECOVERY_IVL interval

  let get_recovery_interval socket =
    get_int_option socket ZMQ_RECOVERY_IVL

  let set_send_buffer_size socket size =
    set_int_option socket ZMQ_SNDBUF size

  let get_send_buffer_size socket =
    get_int_option socket ZMQ_SNDBUF

  let set_receive_buffer_size socket size =
    set_int_option socket ZMQ_RCVBUF size

  let get_receive_buffer_size socket =
    get_int_option socket ZMQ_RCVBUF

  let has_more socket =
    get_int_option socket ZMQ_RCVMORE != 0

  let set_linger_period socket period =
    set_int_option socket ZMQ_LINGER period

  let get_linger_period socket =
    get_int_option socket ZMQ_LINGER

  let set_reconnect_interval socket interval =
    set_int_option socket ZMQ_RECONNECT_IVL interval

  let get_reconnect_interval socket =
    get_int_option socket ZMQ_RECONNECT_IVL

  let set_connection_backlog socket backlog =
    set_int_option socket ZMQ_BACKLOG backlog

  let get_connection_backlog socket =
    get_int_option socket ZMQ_BACKLOG

  let set_reconnect_interval_max socket interval =
    set_int_option socket ZMQ_RECONNECT_IVL_MAX interval

  let get_reconnect_interval_max socket =
    get_int_option socket ZMQ_RECONNECT_IVL_MAX

  let set_send_high_water_mark socket mark =
    set_int_option socket ZMQ_SNDHWM mark

  let get_send_high_water_mark socket =
    get_int_option socket ZMQ_SNDHWM

  let set_receive_high_water_mark socket mark =
    set_int_option socket ZMQ_RCVHWM mark

  let get_receive_high_water_mark socket =
    get_int_option socket ZMQ_RCVHWM

  let set_multicast_hops socket hops =
    set_int_option socket ZMQ_MULTICAST_HOPS hops

  let get_multicast_hops socket =
    get_int_option socket ZMQ_MULTICAST_HOPS

  let set_receive_timeout socket timeout =
    set_int_option socket ZMQ_RCVTIMEO timeout

  let get_receive_timeout socket =
    get_int_option socket ZMQ_RCVTIMEO

  let set_send_timeout socket timeout =
    set_int_option socket ZMQ_SNDTIMEO timeout

  let get_send_timeout socket =
    get_int_option socket ZMQ_SNDTIMEO

  let set_ipv4_only socket flag =
    let value = match flag with true -> 1 | false -> 0 in
    set_int_option socket ZMQ_IPV4ONLY value

  let get_ipv4_only socket =
    match get_int_option socket ZMQ_IPV4ONLY with
    | 0 -> false
    | _ -> true

  let set_router_mandatory socket flag =
    let value = match flag with true -> 1 | false -> 0 in
    set_int_option socket ZMQ_ROUTER_MANDATORY value

  let get_router_mandatory socket =
    match get_int_option socket ZMQ_ROUTER_MANDATORY with
    | 0 -> false
    | _ -> true

  let set_tcp_keepalive socket flag =
    let value = match flag with
      | `Default -> -1
      | `Value false -> 0
      | `Value true -> 1
    in
    set_int_option socket ZMQ_TCP_KEEPALIVE value

  let get_tcp_keepalive socket =
    match get_int_option socket ZMQ_TCP_KEEPALIVE with
    | -1 -> `Default
    | 0 -> `Value false
    | _ -> `Value true

  let set_tcp_keepalive_idle socket flag =
    let value = match flag with
      | `Default -> -1
      | `Value n when n <= 0 -> raise Illegal_argument
      | `Value n -> n
    in
    set_int_option socket ZMQ_TCP_KEEPALIVE_IDLE value

  let get_tcp_keepalive_idle socket =
    match get_int_option socket ZMQ_TCP_KEEPALIVE_IDLE with
    | -1 -> `Default
    | n when n <= 0 -> raise Illegal_argument
    | n -> `Value n

  let set_tcp_keepalive_interval socket flag =
    let value = match flag with
      | `Default -> -1
      | `Value n when n <= 0 -> raise Illegal_argument
      | `Value n -> n
    in
    set_int_option socket ZMQ_TCP_KEEPALIVE_INTVL value

  let get_tcp_keepalive_interval socket =
    match get_int_option socket ZMQ_TCP_KEEPALIVE_INTVL with
    | -1 -> `Default
    | n when n <= 0 -> raise Illegal_argument
    | n -> `Value n


  let set_tcp_keepalive_count socket flag =
    let value = match flag with
      | `Default -> -1
      | `Value n when n <= 0 -> raise Illegal_argument
      | `Value n -> n
    in
    set_int_option socket ZMQ_TCP_KEEPALIVE_CNT value

  let get_tcp_keepalive_count socket =
    match get_int_option socket ZMQ_TCP_KEEPALIVE_CNT with
    | -1 -> `Default
    | n when n <= 0 -> raise Illegal_argument
    | n -> `Value n

  let set_delay_attach_on_connect socket flag =
    let value = match flag with
      | true -> 1
      | false -> 0
    in
    set_int_option socket ZMQ_DELAY_ATTACH_ON_CONNECT value

  let get_delay_attach_on_connect socket =
    match get_int_option socket ZMQ_DELAY_ATTACH_ON_CONNECT with
    | 0 -> false
    | _ -> true

  let set_xpub_verbose socket flag =
    let value = match flag with
      | true -> 1
      | false -> 0
    in
    set_int_option socket ZMQ_XPUB_VERBOSE value

  external get_fd : 'a t -> Unix.file_descr = "caml_zmq_get_fd"

  type event = No_event | Poll_in | Poll_out | Poll_in_out | Poll_error
  external events : 'a t -> event = "caml_zmq_get_events"

end

module Proxy = struct
  external zmq_proxy2 :
    'a Socket.t -> 'b Socket.t -> unit = "caml_zmq_proxy2"
  external zmq_proxy3 :
    'a Socket.t -> 'b Socket.t -> 'c Socket.t -> unit = "caml_zmq_proxy3"

  let create ?capture frontend backend =
    match capture with
    | Some capture -> zmq_proxy3 frontend backend capture
    | None -> zmq_proxy2 frontend backend

end

module Poll = struct

  type t

  type poll_event = In | Out | In_out
  type 'a poll_mask = ('a Socket.t * poll_event)

  external mask_of : 'a poll_mask array -> t = "caml_zmq_poll_of_pollitem_array"
  external native_poll: t -> int -> poll_event option array = "caml_zmq_poll"

  let poll ?(timeout = -1) items = native_poll items timeout

end
