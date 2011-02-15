/*
 * Copyright (c) 2011 Pedro Borges and contributors
 */

#include "caml_zmq/fail.h"

#include <zmq.h>

#include <caml/callback.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>

/* This table must be synchronized with the variant definition. */
static int const caml_zmq_error_table[] = {
    EINVAL,
    EFAULT,
    EMTHREAD,
    ETERM,
    ENODEV,
    EADDRNOTAVAIL,
    EADDRINUSE,
    ENOCOMPATPROTO,
    EPROTONOSUPPORT,
    EAGAIN,
    ENOTSUP,
    EFSM,
    ENOMEM
};

/* This must be the last value of the variant. */
static int const caml_zmq_EUNKNOWN =
    (sizeof caml_zmq_error_table) / (sizeof caml_zmq_error_table[0]);

void caml_zmq_raise_if(int condition) {
    CAMLparam0 ();
    CAMLlocalN(error_parameters, 2);
    if(condition) {
        int error_to_raise = caml_zmq_EUNKNOWN;
        int current_errno = zmq_errno();
        int i;
        for (i = 0; i < caml_zmq_EUNKNOWN; i++) {
            if (current_errno == caml_zmq_error_table[i]) {
                error_to_raise = i;
                break;
            }
        }
        error_parameters[0] = Val_int(error_to_raise);
        error_parameters[1] = caml_copy_string(zmq_strerror(current_errno));
        caml_raise_with_args(
            *caml_named_value("zmq exception"),
            2,
            error_parameters);
    }
    CAMLreturn0;
}

