/*
 * Copyright (c) 2011 Pedro Borges and contributors
 */

#include "socket.h"

#include <caml/memory.h>
#include <caml/custom.h>

static struct custom_operations caml_zmq_socket_ops = {
    "org.zeromq.socket",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
#ifdef custom_compare_ext_default
    , custom_compare_ext_default
#endif
};

value caml_zmq_copy_socket(void *zmq_socket) {
    CAMLparam0 ();
    CAMLlocal1 (socket);
    socket = caml_alloc_custom(&caml_zmq_socket_ops, sizeof (zmq_socket), 0, 1);
    CAML_ZMQ_Socket_val(socket) = zmq_socket;
    CAMLreturn (socket);
}
