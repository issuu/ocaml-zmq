/*
 * Copyright (c) 2011 Pedro Borges and contributors
 */

#include "msg.h"

#include <caml/custom.h>
#include <caml/memory.h>

#include <zmq.h>

static void custom_finalize_msg(value msg) {
    if (CAML_ZMQ_Msg_val(msg)) {
        zmq_msg_close(CAML_ZMQ_Msg_val(msg));
        free(CAML_ZMQ_Msg_val(msg));
        CAML_ZMQ_Msg_val(msg) = NULL;
    }
}

static struct custom_operations caml_zmq_msg_ops = {
    "org.zeromq.msg",
    custom_finalize_msg,
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

value caml_zmq_copy_msg(void *zmq_msg) {
    CAMLparam0 ();
    CAMLlocal1 (msg);
    msg = caml_alloc_custom(&caml_zmq_msg_ops, sizeof (zmq_msg), 0, 1);
    CAML_ZMQ_Msg_val(msg) = zmq_msg;
    CAMLreturn (msg);
}
