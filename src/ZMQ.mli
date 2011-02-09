(**
 Copyright (C) 2011 by Pedro Borges and contributors

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
*)
type context
type socket

exception ZMQ_error of string

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

type setable_getable_socket_option = 
	[`High_water_mark of Uint64.t
        | `Swap of int64
        | `Affinity of Uint64.t
        | `Identity of string
	| `Rate of int64 (** All when using multicast *)
        | `Recovery_interval of int64 (** All when using multicast *)
        | `Multicast_loop of int64 (** All when using multicast *)
        | `Send_buffer of Uint64.t
        | `Recieve_buffer of Uint64.t]

type setable_socket_option =
	[ setable_getable_socket_option
	| `Subscribe of string (** Only for Sub sockets *)
        | `Unsubscribe of string (** Only for Sub sockets *)]

type getable_socket_option = 
	[ setable_getable_socket_option
	| `Recieve_more of int64]

type setable_getable_socket_option_tag =
	[`High_water_mark
        | `Swap
        | `Affinity
        | `Identity
	| `Rate
        | `Recovery_interval
        | `Multicast_loop
        | `Send_buffer
	| `Recieve_buffer]

type getable_socket_option_tag =
	[ setable_getable_socket_option_tag
	| `Recieve_more]
        
type send_recv_option =
        None
        | No_block
        | Snd_more

val version : unit -> int * int * int

val init : int -> context
val term : context -> unit

val socket : context -> socket_kind -> socket
val close : socket -> unit

val setsockopt : socket -> setable_socket_option -> unit
val getsockoption : socket -> getable_socket_option_tag -> getable_socket_option
val bind : socket -> string -> unit
val connect : socket -> string -> unit

val send : socket -> string -> send_recv_option -> unit
val recv : socket -> send_recv_option -> string

