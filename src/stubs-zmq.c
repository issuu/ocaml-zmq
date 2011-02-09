/*
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
 */

#include "zmq.h"

#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/intext.h>
#include <caml/threads.h>

/*static int zmq_errno_to_caml_error(int zmq_errno) {
	if() TODO: raise proper error type
	return caml_error;
}*/


void caml_zmq_raise_if(int condition) {
	if(condition) {
		int zmq_err = zmq_errno();
		caml_raise_with_string(*caml_named_value("zmq exception"), zmq_strerror(zmq_err));
	}	
}

CAMLprim value caml_zmq_version(value unit) {
	CAMLparam1 (unit);
	CAMLlocal1 (version_tuple);

	int major, minor, patch;
	zmq_version(&major, &minor, &patch);

	version_tuple = caml_alloc_tuple(3);
	Store_field(version_tuple, 0, Val_int(major));
	Store_field(version_tuple, 1, Val_int(minor));
	Store_field(version_tuple, 2, Val_int(patch));

	CAMLreturn (version_tuple);
}

static struct custom_operations zmq_context_ops = {
	"org.zeromq.context",
	custom_finalize_default,
	custom_compare_default,
	custom_hash_default,
	custom_serialize_default,
	custom_deserialize_default
};

#define ZMQ_context_val(v) (*((void **) Data_custom_val(v)))

static value caml_alloc_zmq_context(void *context) {
	CAMLparam0 ();
	CAMLlocal1 (ctx);
	ctx = caml_alloc_custom(&zmq_context_ops, sizeof (context), 0, 1);
	ZMQ_context_val(ctx) = context;
	CAMLreturn (ctx);
}

CAMLprim value caml_zmq_init(value num_threads) {
	CAMLparam1 (num_threads);
	CAMLlocal1 (ctx_value);
	void *ctx = zmq_init(Int_val(num_threads));
	caml_zmq_raise_if(ctx == NULL);
	ctx_value = caml_alloc_zmq_context(ctx);
	CAMLreturn (ctx_value);
}

CAMLprim value caml_zmq_term(value ctx) {
	CAMLparam1 (ctx);
	int result = zmq_term(ZMQ_context_val(ctx));
	caml_zmq_raise_if(result == -1);
	CAMLreturn (Val_unit);
}

static struct custom_operations zmq_socket_ops = {
	"org.zeromq.socket",
	custom_finalize_default,
	custom_compare_default,
	custom_hash_default,
	custom_serialize_default,
	custom_deserialize_default
};

#define ZMQ_socket_val(v) (*((void **) Data_custom_val(v)))

static value caml_alloc_zmq_socket(void *socket) {
	CAMLparam0 ();
	CAMLlocal1 (sock);
	sock = caml_alloc_custom(&zmq_socket_ops, sizeof (socket), 0, 1);
	ZMQ_context_val(sock) = socket;
	CAMLreturn (sock);
}


/* Order must match OCaml's variant declaration */
static int socket_type_for_kind[] =  {
	ZMQ_PAIR,
	ZMQ_PUB,
	ZMQ_SUB,
	ZMQ_REQ,
	ZMQ_REP,
	ZMQ_XREQ,
	ZMQ_XREP,
	ZMQ_PULL,
	ZMQ_PUSH
};

CAMLprim value caml_zmq_socket(value ctx, value socket_kind) {
	CAMLparam2 (ctx, socket_kind);
	CAMLlocal1 (sock_value);
	void *socket = zmq_socket(ZMQ_context_val(ctx), socket_type_for_kind[Int_val(socket_kind)]);
	caml_zmq_raise_if(socket == NULL);
	sock_value = caml_alloc_zmq_socket(socket);
	CAMLreturn (sock_value);
}

CAMLprim value caml_zmq_close(value socket) {
	CAMLparam1 (socket);
	int result = zmq_close(ZMQ_socket_val(socket));
	caml_zmq_raise_if(result == -1);
	CAMLreturn (Val_unit);
}

#define Uint64_val(v) (*((uint64 *)Data_custom_val(v)))

CAMLprim value caml_zmq_setsockopt(value socket, value sockopt) {
	CAMLparam2 (socket, sockopt);
	int result;
	int option_name = -1; /* Invalid option. zmq_setsockopt will catch it later. */
	void *option_value = NULL;
	size_t option_size = 0;
	
	if (caml_hash_variant("High_water_mark") == Field(0, sockopt)) {
		option_name = ZMQ_HWM;
		uint64 mark = Uint64_val(Field(sockopt, 1));
		option_value = (void *) &mark;
		option_size = sizeof (mark);
	} else if (caml_hash_variant("Swap") == Field(0, sockopt)) {
		option_name = ZMQ_SWAP;
		int64 swap = Int64_val(Field(sockopt, 1));
		option_value = (void *) &swap;
		option_size = sizeof (swap);
	} else if (caml_hash_variant("Affinity") == Field(0, sockopt)) {
		option_name = ZMQ_AFFINITY;
		uint64 affinity = Uint64_val(Field(sockopt, 1));
		option_value = (void *) &affinity;
		option_size = sizeof (affinity);
	} else if (caml_hash_variant("Identity") == Field(0, sockopt)) {
		option_name = ZMQ_IDENTITY;
		char *identity = String_val(Field(sockopt, 1));
		option_value = (void *) identity;
		option_size = strlen(identity);
	} else if (caml_hash_variant("Subscribe") == Field(0, sockopt)) {
		option_name = ZMQ_SUBSCRIBE;
		char *subs = String_val(Field(sockopt, 1));
		option_value = (void *) subs;
		option_size = strlen(subs);
	} else if (caml_hash_variant("Unsubscribe") == Field(0, sockopt)) {
		option_name = ZMQ_UNSUBSCRIBE;
		char *unsubs = String_val(Field(sockopt, 1));
		option_value = (void *) unsubs;
		option_size = strlen(unsubs);
	} else if (caml_hash_variant("Rate") == Field(0, sockopt)) {
		option_name = ZMQ_RATE;
		int64 rate = Int64_val(Field(sockopt, 1));
		option_value = (void *) &rate;
		option_size = sizeof (rate);
	} else if (caml_hash_variant("Recovery_interval") == Field(0, sockopt)) {
		option_name = ZMQ_RECOVERY_IVL;
		int64 recovery = Int64_val(Field(sockopt, 1));
		option_value = (void *) &recovery;
		option_size = sizeof (recovery);
	} else if (caml_hash_variant("Multicast_loop") == Field(0, sockopt)) {
		option_name = ZMQ_MCAST_LOOP;
		int64 mloop = Int64_val(Field(sockopt, 1));
		option_value = (void *) &mloop;
		option_size = sizeof (mloop);
	} else if (caml_hash_variant("Send_buffer") == Field(0, sockopt)) {
		option_name = ZMQ_SNDBUF;
		uint64 sbuf = Uint64_val(Field(sockopt, 1));
		option_value = (void *) &sbuf;
		option_size = sizeof (sbuf);
	} else if (caml_hash_variant("Recieve_buffer") == Field(0, sockopt)) {
		option_name = ZMQ_RCVBUF;
		uint64 rbuf = Uint64_val(Field(sockopt, 1));
		option_value = (void *) &rbuf;
		option_size = sizeof (rbuf);
	}

	result = zmq_setsockopt(ZMQ_socket_val(socket), option_name, option_value, option_size);
	caml_zmq_raise_if(result == -1);
	CAMLreturn (Val_unit);
}

/* copy contents from uint https://github.com/andrenth/ocaml-uint */

static int
uint64_cmp(value v1, value v2)
{
    uint64 i1 = Uint64_val(v1);
    uint64 i2 = Uint64_val(v2);
    return (i1 > i2) - (i1 < i2);
}

static intnat
uint64_hash(value v)
{
    return((intnat)Uint64_val(v));
}

static void
uint64_serialize(value v, uintnat *wsize_32, uintnat *wsize_64)
{
    caml_serialize_int_8(Uint64_val(v));
    *wsize_32 = *wsize_64 = 8;
}

static uintnat
uint64_deserialize(void *dst)
{
    *((uint64 *) dst) = caml_deserialize_uint_8();
    return 8;
}

struct custom_operations uint64_ops = {
    "uint.uint64",
    custom_finalize_default,
    uint64_cmp,
    uint64_hash,
    uint64_serialize,
    uint64_deserialize
};

CAMLprim value
copy_uint64(uint64 i)
{
    CAMLparam0();
    value res = caml_alloc_custom(&uint64_ops, 8, 0, 1);
    Uint64_val(res) = i;
    CAMLreturn (res);
};


/* end copy contents from uint */


CAMLprim value caml_zmq_getsockoption(value socket, value socket_option_tag) {
	CAMLparam2 (socket, socket_option_tag);
	CAMLlocal2 (option, option_contents);
	int result;
	option = caml_alloc_tuple(2);
	Store_field(option, 0, socket_option_tag);
	if (caml_hash_variant("High_water_mark") == socket_option_tag) {
		uint64 mark;
		size_t mark_size = sizeof (mark);
		result = zmq_getsockopt (ZMQ_socket_val(socket), ZMQ_HWM, &mark, &mark_size);
		caml_zmq_raise_if (result == -1);
		option_contents = copy_uint64(mark);
	} else if (caml_hash_variant("Swap") == socket_option_tag) {
		int64 swap;
		size_t swap_size = sizeof (swap);
		result = zmq_getsockopt(ZMQ_socket_val(socket), ZMQ_SWAP, &swap, &swap_size);
		caml_zmq_raise_if (result == -1);
		option_contents = caml_copy_int64(swap);
	} else if (caml_hash_variant("Affinity") == socket_option_tag) {
		uint64 affinity;
		size_t affinity_size = sizeof (affinity);
		result = zmq_getsockopt (ZMQ_socket_val(socket), ZMQ_AFFINITY, &affinity, &affinity_size);
		caml_zmq_raise_if (result == -1);
		option_contents = copy_uint64(affinity);
	} else if (caml_hash_variant("Identity") == socket_option_tag) {
		char buffer[256]; /* max is 255 */
		size_t buffer_size = sizeof (buffer);
		result = zmq_getsockopt (ZMQ_socket_val(socket), ZMQ_IDENTITY, buffer, &buffer_size);
		buffer[result] = '\0'; /* always inside */
		option_contents = caml_copy_string(buffer);
	} else if (caml_hash_variant("Rate") == socket_option_tag) {
		int64 rate;
		size_t rate_size = sizeof (rate);
		result = zmq_getsockopt(ZMQ_socket_val(socket), ZMQ_RATE, &rate, &rate_size);
		caml_zmq_raise_if (result == -1);
		option_contents = caml_copy_int64(rate);
	} else if (caml_hash_variant("Recovery_interval") == socket_option_tag) {
		int64 recovery;
		size_t recovery_size = sizeof (recovery);
		result = zmq_getsockopt(ZMQ_socket_val(socket), ZMQ_RECOVERY_IVL, &recovery, &recovery_size);
		caml_zmq_raise_if (result == -1);
		option_contents = caml_copy_int64(recovery);
	} else if (caml_hash_variant("Multicast_loop") == socket_option_tag) {
		int64 mloop;
		size_t mloop_size = sizeof (mloop);
		result = zmq_getsockopt(ZMQ_socket_val(socket), ZMQ_MCAST_LOOP, &mloop, &mloop_size);
		caml_zmq_raise_if (result == -1);
		option_contents = caml_copy_int64(mloop);
	} else if (caml_hash_variant("Send_buffer") == socket_option_tag) {
		uint64 sbuffer;
		size_t sbuffer_size = sizeof (sbuffer);
		result = zmq_getsockopt (ZMQ_socket_val(socket), ZMQ_SNDBUF, &sbuffer, &sbuffer_size);
		caml_zmq_raise_if (result == -1);
		option_contents = copy_uint64(sbuffer);
	} else if (caml_hash_variant("Recieve_buffer") == socket_option_tag) {
		uint64 rbuffer;
		size_t rbuffer_size = sizeof (rbuffer);
		result = zmq_getsockopt (ZMQ_socket_val(socket), ZMQ_RCVBUF, &rbuffer, &rbuffer_size);
		caml_zmq_raise_if (result == -1);
		option_contents = copy_uint64(rbuffer);
	} else if (caml_hash_variant("Recieve_more") == socket_option_tag) {
		int64 more;
		size_t more_size = sizeof (more);
		result = zmq_getsockopt(ZMQ_socket_val(socket), ZMQ_RCVMORE, &more, &more_size);
		caml_zmq_raise_if (result == -1);
		option_contents = caml_copy_int64(more);
	} else {
		/* cannot happen */
	}
	Store_field(option, 1, option_contents);
	CAMLreturn (option);
}

CAMLprim value caml_zmq_bind(value socket, value string_address) {
	CAMLparam2 (socket, string_address);
	
	int result = zmq_bind(ZMQ_socket_val(socket), String_val(string_address));
	caml_zmq_raise_if (result == -1);

	CAMLreturn(Val_unit);
}

CAMLprim value caml_zmq_connect(value socket, value string_address) {
	CAMLparam2 (socket, string_address);
	
	int result = zmq_connect(ZMQ_socket_val(socket), String_val(string_address));
	caml_zmq_raise_if (result == -1);

	CAMLreturn(Val_unit);
}

static int snd_rcv_options_table[] = {
	0,
	ZMQ_NOBLOCK,
	ZMQ_SNDMORE
};

CAMLprim value caml_zmq_send(value socket, value string, value snd_options) {
	CAMLparam3 (socket, string, snd_options);
	void *sock = ZMQ_socket_val(socket);
	int option = snd_rcv_options_table[Int_val(snd_options)];
	zmq_msg_t msg;
	int result = zmq_msg_init_size(&msg, caml_string_length(string));
	caml_zmq_raise_if (result == -1);
	/* Doesn't copy '\0' */
	memcpy ((void *) zmq_msg_data (&msg), String_val(string), caml_string_length(string));

	caml_release_runtime_system();
	result = zmq_send (sock, &msg, option);
	caml_acquire_runtime_system();
	caml_zmq_raise_if (result == -1);
    result = zmq_msg_close (&msg);
	caml_zmq_raise_if (result == -1);
	
	CAMLreturn(Val_unit);
}

CAMLprim value caml_zmq_recv(value socket, value rcv_options) {
	CAMLparam2 (socket, rcv_options);
	CAMLlocal1 (message);
	zmq_msg_t request;
	void *sock = ZMQ_socket_val(socket);
	int option = snd_rcv_options_table[Int_val(rcv_options)];
	int result = zmq_msg_init (&request);
	caml_zmq_raise_if (result == -1);

	caml_release_runtime_system();
	result = zmq_recv (sock, &request, option);
	caml_acquire_runtime_system();
	caml_zmq_raise_if (result == -1);

	size_t size = zmq_msg_size (&request);
	message = caml_alloc_string(size + 1);
	memcpy (String_val(message), zmq_msg_data (&request), size);
	String_val(message)[size] = '\0';
	CAMLreturn (message);
}

