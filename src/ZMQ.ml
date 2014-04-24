(* Copyright (c) 2011 Pedro Borges and contributors *)

(** Module Exceptions *)

type error =
| EFSM
| ENOCOMPATPROTO
| ETERM
| EMTHREAD
| EUNKNOWN

exception ZMQ_exception of error * string

let _ =
  Callback.register_exception "ZMQ.ZMQ_exception" (ZMQ_exception(EUNKNOWN, "Unknown error"))

external version : unit -> int * int * int = "caml_zmq_version"

module Context = struct
  type t

  external create : unit -> t = "caml_zmq_new"
  external terminate : t -> unit = "caml_zmq_term"

  type int_option =
  | ZMQ_IO_THREADS
  | ZMQ_MAX_SOCKETS
  | ZMQ_IPV6

  external set_int_option :
    t -> int_option -> int -> unit = "caml_zmq_ctx_set_int_option"
  external get_int_option :
    t -> int_option -> int = "caml_zmq_ctx_get_int_option"

  let get_io_threads ctx =
    get_int_option ctx ZMQ_IO_THREADS

  let set_io_threads ctx =
    set_int_option ctx ZMQ_IO_THREADS

  let get_max_sockets ctx =
    get_int_option ctx ZMQ_MAX_SOCKETS

  let set_max_sockets ctx =
    set_int_option ctx ZMQ_MAX_SOCKETS

  let get_ipv6 ctx =
    (get_int_option ctx ZMQ_IPV6) == 1

  let set_ipv6 ctx has_ipv6 =
    set_int_option ctx ZMQ_IPV6 (if has_ipv6 then 1 else 0)

end

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
  let stream = 11

  (** Creation and Destruction *)
  external create : Context.t -> 'a kind -> 'a t = "caml_zmq_socket"
  external close : 'a t -> unit = "caml_zmq_close"

  (** Wiring *)
  external connect : 'a t -> string -> unit = "caml_zmq_connect"
  external disconnect : 'a t -> string -> unit = "caml_zmq_disconnect"
  external bind : 'a t -> string -> unit = "caml_zmq_bind"

  (** Send and Receive *)
  external native_recv : 'a t -> bool -> string = "caml_zmq_recv"
  let recv ?(block = true) socket = native_recv socket block

  external native_send : 'a t -> string -> bool -> bool -> unit = "caml_zmq_send"
  let send ?(block = true) ?(more = false) socket message = native_send socket message block more

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
  | ZMQ_PLAIN_USERNAME
  | ZMQ_PLAIN_PASSWORD
  | ZMQ_CURVE_PUBLICKEY
  | ZMQ_CURVE_SECRETKEY
  | ZMQ_CURVE_SERVERKEY
  | ZMQ_ZAP_DOMAIN

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
  | ZMQ_IPV6
  | ZMQ_ROUTER_MANDATORY
  | ZMQ_TCP_KEEPALIVE
  | ZMQ_TCP_KEEPALIVE_CNT
  | ZMQ_TCP_KEEPALIVE_IDLE
  | ZMQ_TCP_KEEPALIVE_INTVL
  | ZMQ_IMMEDIATE
  | ZMQ_XPUB_VERBOSE
  | ZMQ_MECHANISM
  | ZMQ_PLAIN_SERVER
  | ZMQ_CURVE_SERVER
  | ZMQ_PROBE_ROUTER
  | ZMQ_REQ_CORRELATE
  | ZMQ_REQ_RELAXED
  | ZMQ_CONFLATE

  external set_int_option :
    'a t -> int_option -> int -> unit = "caml_zmq_set_int_option"

  external get_int_option :
    'a t -> int_option -> int = "caml_zmq_get_int_option"


  let validate_string_length min max str msg =
    match String.length str with
    | n when n < min -> invalid_arg msg
    | n when n > max -> invalid_arg msg
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
    validate_string_length 1 255 identity "set_identity";
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

  let set_ipv6 socket flag =
    let value = match flag with true -> 1 | false -> 0 in
    set_int_option socket ZMQ_IPV6 value

  let get_ipv6 socket =
    match get_int_option socket ZMQ_IPV6 with
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
      | `Value n when n <= 0 -> invalid_arg "set_tcp_keepalive_idle"
      | `Value n -> n
    in
    set_int_option socket ZMQ_TCP_KEEPALIVE_IDLE value

  let get_tcp_keepalive_idle socket =
    match get_int_option socket ZMQ_TCP_KEEPALIVE_IDLE with
    | -1 -> `Default
    | n when n <= 0 -> assert false
    | n -> `Value n

  let set_tcp_keepalive_interval socket flag =
    let value = match flag with
      | `Default -> -1
      | `Value n when n <= 0 -> invalid_arg "set_tcp_keepalive_interval"
      | `Value n -> n
    in
    set_int_option socket ZMQ_TCP_KEEPALIVE_INTVL value

  let get_tcp_keepalive_interval socket =
    match get_int_option socket ZMQ_TCP_KEEPALIVE_INTVL with
    | -1 -> `Default
    | n when n <= 0 -> assert false
    | n -> `Value n

  let set_tcp_keepalive_count socket flag =
    let value = match flag with
      | `Default -> -1
      | `Value n when n <= 0 -> invalid_arg "set_tcp_keepalive_count"
      | `Value n -> n
    in
    set_int_option socket ZMQ_TCP_KEEPALIVE_CNT value

  let get_tcp_keepalive_count socket =
    match get_int_option socket ZMQ_TCP_KEEPALIVE_CNT with
    | -1 -> `Default
    | n when n <= 0 -> assert false
    | n -> `Value n

  let set_immediate socket flag =
    let value = match flag with
      | true -> 1
      | false -> 0
    in
    set_int_option socket ZMQ_IMMEDIATE value

  let get_immediate socket =
    match get_int_option socket ZMQ_IMMEDIATE with
    | 0 -> false
    | _ -> true

  let set_xpub_verbose socket flag =
    let value = match flag with
      | true -> 1
      | false -> 0
    in
    set_int_option socket ZMQ_XPUB_VERBOSE value

  let set_probe_router socket flag =
    set_int_option socket ZMQ_PROBE_ROUTER (if flag then 1 else 0)

  let set_req_correlate socket flag =
    set_int_option socket ZMQ_REQ_CORRELATE (if flag then 1 else 0)

  let set_req_relaxed socket flag =
    set_int_option socket ZMQ_REQ_RELAXED (if flag then 1 else 0)

  let set_plain_server socket flag =
    set_int_option socket ZMQ_PLAIN_SERVER (if flag then 1 else 0)

  let set_curve_server socket flag =
    set_int_option socket ZMQ_CURVE_SERVER (if flag then 1 else 0)

  let set_plain_username socket =
    set_bytes_option socket ZMQ_PLAIN_USERNAME

  let get_plain_username socket =
    get_bytes_option socket ZMQ_PLAIN_USERNAME

  let set_plain_password socket =
    set_bytes_option socket ZMQ_PLAIN_PASSWORD

  let get_plain_password socket =
    get_bytes_option socket ZMQ_PLAIN_PASSWORD

  let validate_curve_key_length str msg =
    match String.length str with
    | 32 | 40 -> ()
    | _ -> invalid_arg msg

  let get_curve_publickey socket =
    get_bytes_option socket ZMQ_CURVE_PUBLICKEY

  let set_curve_publickey socket str =
    validate_curve_key_length str "set_curve_publickey";
    set_bytes_option socket ZMQ_CURVE_PUBLICKEY str

  let get_curve_secretkey socket =
    get_bytes_option socket ZMQ_CURVE_SECRETKEY

  let set_curve_secretkey socket str =
    validate_curve_key_length str "set_curve_secretkey";
    set_bytes_option socket ZMQ_CURVE_SECRETKEY str

  let get_curve_serverkey socket =
    get_bytes_option socket ZMQ_CURVE_SERVERKEY

  let set_curve_serverkey socket str =
    validate_curve_key_length str "set_curve_serverkey";
    set_bytes_option socket ZMQ_CURVE_SERVERKEY str

  let get_mechanism socket =
    match get_int_option socket ZMQ_MECHANISM with
    | 0 -> `Null
    | 1 -> `Plain
    | 2 -> `Curve
    | _ -> assert false

  let set_zap_domain socket =
    set_bytes_option socket ZMQ_ZAP_DOMAIN

  let get_zap_domain socket =
    get_bytes_option socket ZMQ_ZAP_DOMAIN

  let set_conflate socket flag =
    set_int_option socket ZMQ_CONFLATE (if flag then 1 else 0)

  external get_fd : 'a t -> Unix.file_descr = "caml_zmq_get_fd"

  type event = No_event | Poll_in | Poll_out | Poll_in_out | Poll_error
  external events : 'a t -> event = "caml_zmq_get_events"

  let recv_all =
    (* Once the first message part is received all remaining message parts can
       be received without blocking. *)
    let rec loop socket accu =
      if has_more socket then
        loop socket (recv socket :: accu)
      else
        accu
    in
    fun ?block socket ->
      let first = recv ?block socket in
      List.rev (loop socket [first])

  let send_all =
    (* Once the first message part is sent all remaining message parts can
       be sent without blocking. *)
    let rec send_all_inner_loop socket message =
      match message with
      | [] -> ()
      | hd :: [] ->
        send socket hd
      | hd :: tl ->
        send ~more:true socket hd;
        send_all_inner_loop socket tl
    in
    fun ?block socket message ->
      match message with
      | [] -> ()
      | hd :: [] ->
        send ?block ~more:false socket hd
      | hd :: tl ->
        send ?block ~more:true socket hd;
        send_all_inner_loop socket tl
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

module Monitor = struct
  type t = string

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

  external socket_monitor: 'a Socket.t -> string -> unit = "caml_zmq_socket_monitor"

  let create socket =
    (* Construct an anonymous inproc channel name *)
    let socket_id = Hashtbl.hash (Socket.get_fd socket) in
    let address = Printf.sprintf "inproc://_socket_monitor-%d-%x.%x"
      (Unix.getpid ())
      socket_id
      (Random.bits ())
    in
    socket_monitor socket address;
    address

  let connect ctx t =
    let s = Socket.create ctx Socket.pair in
    Socket.connect s t;
    s

  external decode_monitor_event : string -> string -> event = "caml_decode_monitor_event"

  let recv ?block socket =
    let event = Socket.recv ?block socket in
    assert (Socket.has_more socket);
    let addr = Socket.recv ~block:false socket in
    decode_monitor_event event addr

  let get_peer_address fd =
    try
      let sockaddr = Unix.getpeername fd in
      let domain = match Unix.domain_of_sockaddr sockaddr with
        | Unix.PF_UNIX -> "unix"
        | Unix.PF_INET -> "tcp"
        | Unix.PF_INET6 -> "tcp6"
      in
      match sockaddr with
      | Unix.ADDR_UNIX s -> Printf.sprintf "%s://%s" domain s;
      | Unix.ADDR_INET (addr, port) -> Printf.sprintf "%s://%s:%d" domain (Unix.string_of_inet_addr addr) port
    with
    | Unix.Unix_error _ -> "unknown"

  let internal_string_of_event push_address pop_address = function
    | Connected (addr, fd) -> Printf.sprintf "Connect: %s. peer %s" addr (push_address fd)
    | Connect_delayed addr -> Printf.sprintf "Connect delayed: %s" addr
    | Connect_retried (addr, interval) -> Printf.sprintf "Connect retried: %s - %d" addr interval
    | Listening (addr, fd) -> Printf.sprintf "Listening: %s - peer %s" addr (push_address fd)
    | Bind_failed (addr, error_no, error_text) -> Printf.sprintf "Bind failed: %s. %d:%s" addr error_no error_text
    | Accepted (addr, fd) -> Printf.sprintf "Accepted: %s. peer %s" addr (push_address fd)
    | Accept_failed (addr, error_no, error_text) -> Printf.sprintf "Accept failed: %s. %d:%s" addr error_no error_text
    | Closed (addr, fd) -> Printf.sprintf "Closed: %s. peer %s" addr (pop_address fd)
    | Close_failed (addr, error_no, error_text) -> Printf.sprintf "Close failed: %s. %d:%s" addr error_no error_text
    | Disconnected (addr, fd) -> Printf.sprintf "Disconnect: %s. peer %s" addr (pop_address fd)

  let string_of_event event = internal_string_of_event get_peer_address get_peer_address event

  let mk_string_of_event () =
    let state = ref [] in

    let pop_address fd =
      let rec pop acc = function
        | [] -> (get_peer_address fd, acc)
        | (fd', address) :: xs when fd' = fd -> (address, acc @ xs)
        | x :: xs -> pop (x :: acc) xs
      in
      let (address, new_state) = pop [] !state in
      state := new_state;
      address
    in

    let push_address fd =
      let address = get_peer_address fd in
      state := (fd, address) :: !state;
      address
    in
    internal_string_of_event push_address pop_address

end

module Z85 = struct
  external encode : string -> string = "caml_z85_encode"
  external decode : string -> string = "caml_z85_decode"
end

module Curve = struct
  external keypair : unit -> string * string = "caml_curve_keypair"
end
