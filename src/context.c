/*
 * Copyright (c) 2011 Pedro Borges and contributors
 */

#include "context.h"

#include <caml/custom.h>
#include <caml/memory.h>

#include <zmq.h>

CAMLextern value caml_zmq_term(value ctx);

static void custom_finalize_context(value context) {
    if (CAML_ZMQ_Context_val(context)) {
        do {
            int result = zmq_ctx_term(CAML_ZMQ_Context_val(context));
            /* If termination was interrupted by a signal, restart. */
            if (result == EINTR) continue;

            /* For errors other than EINTR (i.e. only EFAULT), see the
               ntoes in socket.c, custom_finalize_socket. */
        } while(0);

        CAML_ZMQ_Context_val(context) = NULL;
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
};

value caml_zmq_copy_context(void *zmq_context) {
    CAMLparam0 ();
    CAMLlocal1 (context);
    context = caml_alloc_custom(&caml_zmq_context_ops, sizeof (zmq_context), 0, 1);
    CAML_ZMQ_Context_val(context) = zmq_context;
    CAMLreturn (context);
}

