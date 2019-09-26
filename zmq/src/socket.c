/*
 * Copyright (c) 2011 Pedro Borges and contributors
 */

#include "socket.h"
#include "fail.h"

#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/alloc.h>
#include <zmq.h>

static void custom_finalize_socket(value socket) {
    if (CAML_ZMQ_Socket_val(socket)) {
        fprintf(stderr, "Error: Socket not closed before finalization\n");
    }
}

static struct custom_operations caml_zmq_socket_ops = {
    "org.zeromq.socket",
    custom_finalize_socket,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
#ifdef custom_compare_ext_default
    , custom_compare_ext_default
#endif
#ifdef custom_fixed_length_default
    , custom_fixed_length_default
#endif
};

/* Captures a reference to the context to avoid collecting it prematurely */
value caml_zmq_copy_socket(void *zmq_socket) {
    CAMLparam0 ();
    CAMLlocal1 (socket);
    socket = caml_alloc_custom(&caml_zmq_socket_ops, sizeof (zmq_socket), 0, 1);
    CAML_ZMQ_Socket_val(socket) = zmq_socket;
    CAMLreturn (socket);
}
