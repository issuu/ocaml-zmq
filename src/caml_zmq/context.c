/*
 * Copyright (c) 2011 Pedro Borges and contributors
 */

#include "caml_zmq/context.h"

#include <caml/custom.h>
#include <caml/memory.h>

static struct custom_operations caml_zmq_context_ops = {
    "org.zeromq.context",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

value caml_zmq_copy_context(void *zmq_context) {
    CAMLparam0 ();
    CAMLlocal1 (context);
    context = caml_alloc_custom(&caml_zmq_context_ops, sizeof (zmq_context), 0, 1);
    CAML_ZMQ_Context_val(context) = zmq_context;
    CAMLreturn (context);
}

