/*
 * Copyright (c) 2011 Pedro Borges and contributors
 */

#include "fail.h"

#include <zmq.h>

#include <caml/callback.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>

/* This table must be synchronized with Zmq.internal_error. */
static int const caml_zmq_error_table[] = {
    ENOTSUP,
    EPROTONOSUPPORT,
    ENOBUFS,
    ENETDOWN,
    EADDRINUSE,
    EADDRNOTAVAIL,
    ECONNREFUSED,
    EINPROGRESS,
    ENOTSOCK,
    EMSGSIZE,
    EAFNOSUPPORT,
    ENETUNREACH,
    ECONNABORTED,
    ECONNRESET,
    ENOTCONN,
    ETIMEDOUT,
    EHOSTUNREACH,
    ENETRESET,
    EFSM,
    ENOCOMPATPROTO,
    ETERM,
    EMTHREAD,
};

/* This must be the last value of the variant. */
static int const caml_zmq_EUNKNOWN =
    (sizeof caml_zmq_error_table) / (sizeof caml_zmq_error_table[0]);

void caml_zmq_raise(int err_no, const char *err_str, const char *name) {
    CAMLparam0 ();

    /* err_no can be a standard Unix error code, or it can be a ZMQ-defined
     * error code in the range above ZMQ_HAUSNUMERO. If the system headers are
     * missing certain Unix codes, they get redefined in zmq.h with a number
     * above ZMQ_HAUSNUMERO. That range also contains new ZMQ-specific error
     * codes.
     */
    if (err_no < ZMQ_HAUSNUMERO) {
        unix_error(err_no, (char *) name, Nothing);

    } else {
        int error_to_raise = caml_zmq_EUNKNOWN;
        int i;
        for (i = 0; i < caml_zmq_EUNKNOWN; i++) {
            if (err_no == caml_zmq_error_table[i]) {
                error_to_raise = i;
                break;
            }
        }
        /* From http://caml.inria.fr/pub/docs/manual-ocaml-4.01/intfc.html#sec439:

            If the function f does not return, but raises an exception that
            escapes the scope of the application, then this exception is
            propagated to the next enclosing OCaml code, skipping over the C
            code. That is, if an OCaml function f calls a C function g that calls
            back an OCaml function h that raises a stray exception, then the
            execution of g is interrupted and the exception is propagated back
            into f.
        */

        caml_callback3(*caml_named_value("Zmq.zmq_raise"),
                       Val_int(error_to_raise),
                       caml_copy_string(err_str),
                       caml_copy_string(name)
                       );
    }

    CAMLreturn0;
}

void caml_zmq_raise_if(int condition, const char *location) {
    if (condition) {
        int err_no = zmq_errno();
        const char *err_str = zmq_strerror(err_no);
        caml_zmq_raise(err_no, err_str, location);
    }
}
