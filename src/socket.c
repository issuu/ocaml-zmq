/*
 * Copyright (c) 2011 Pedro Borges and contributors
 */

#include "socket.h"
#include "fail.h"

#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/alloc.h>
#include <zmq.h>

#define CAML_ZMQ_Socket_inner_val(v) (*(void**)Data_custom_val(v))

static void custom_finalize_socket(value socket) {
    if (CAML_ZMQ_Socket_inner_val(socket)) {
        /* First, set ZMQ_LINGER to 0, so that zmq_term will never hang due to
           finalised sockets. */

        int linger = 0;
        zmq_setsockopt(CAML_ZMQ_Socket_inner_val(socket), ZMQ_LINGER, &linger, sizeof(linger));
        /* See the note on errors below; it applies to EINVAL, ETERM and
           ENOTSOCK. Setting ZMQ_LINGER cannot return EINTR. */

        zmq_close(CAML_ZMQ_Socket_inner_val(socket));
        /* zmq_close can return an error, which will be ignored;
           the only possible error is ENOTSOCK, which would never
           dynamically happen. */

        CAML_ZMQ_Socket_inner_val(socket) = NULL;
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
};

/* Captures a reference to the context to avoid collecting it prematurely */
value caml_zmq_copy_socket(value context, void *zmq_socket) {
    CAMLparam0 ();
    CAMLlocal2 (socket, tuple);

    socket = caml_alloc_custom(&caml_zmq_socket_ops, sizeof (zmq_socket), 0, 1);
    CAML_ZMQ_Socket_inner_val(socket) = zmq_socket;

    tuple = caml_alloc_tuple(2);
    Field(tuple, 0) = context;
    Field(tuple, 1) = socket;

    CAMLreturn (tuple);
}
