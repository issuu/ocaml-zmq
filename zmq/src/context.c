/*
 * Copyright (c) 2011 Pedro Borges and contributors
 */

#include "context.h"

#include <caml/custom.h>
#include <caml/memory.h>

#include <zmq.h>

static void custom_finalize_context(value context) {
    if (CAML_ZMQ_Context_val(context)) {
        fprintf(stderr, "Error: Context not closed before finalization\n");
    }
}

static struct custom_operations caml_zmq_context_ops = {
    "org.zeromq.context",
    custom_finalize_context,
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

value caml_zmq_copy_context(void *zmq_context) {
    CAMLparam0 ();
    CAMLlocal1 (context);
    context = caml_alloc_custom(&caml_zmq_context_ops, sizeof (zmq_context), 0, 1);
    CAML_ZMQ_Context_val(context) = zmq_context;
    CAMLreturn (context);
}
