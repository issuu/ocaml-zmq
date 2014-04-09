/*
 * Copyright (c) 2011 Pedro Borges and contributors
 */

#include "fail.h"

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
    ENOMEM,
    EINTR
};

/* This must be the last value of the variant. */
static int const caml_zmq_EUNKNOWN =
    (sizeof caml_zmq_error_table) / (sizeof caml_zmq_error_table[0]);

void caml_zmq_raise(int err_no, const char *err_str) {
    CAMLparam0 ();
    CAMLlocalN(error_parameters, 2);
    int error_to_raise = caml_zmq_EUNKNOWN;
    int i;
    for (i = 0; i < caml_zmq_EUNKNOWN; i++) {
        if (err_no == caml_zmq_error_table[i]) {
            error_to_raise = i;
            break;
        }
    }
    error_parameters[0] = Val_int(error_to_raise);
    error_parameters[1] = caml_copy_string(err_str);
    caml_raise_with_args(
                         *caml_named_value("zmq exception"),
                         2,
                         error_parameters);
    CAMLreturn0;
}

void caml_zmq_raise_if(int condition) {
    if (condition) {
        int err_no = zmq_errno();
        const char *err_str = zmq_strerror(err_no);
        caml_zmq_raise(err_no, err_str);
    }
}

void caml_zmq_raise_illegal_arg() {
    caml_raise_constant(*caml_named_value("zmq illegal argument"));
}
