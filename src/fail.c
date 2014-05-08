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

/* This table must be synchronized with ZMQ.internal_error. */
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

void caml_zmq_raise(int err_no, const char *err_str) {
    CAMLparam0 ();
    CAMLlocalN(error_parameters, 2);

    /* err_no can be a standard Unix error code, or it can be a ZMQ-defined
     * error code in the range above ZMQ_HAUSNUMERO. If the system headers are
     * missing certain Unix codes, they get redefined in zmq.h with a number
     * above ZMQ_HAUSNUMERO. That range also contains new ZMQ-specific error
     * codes.
     */
    if (err_no < ZMQ_HAUSNUMERO) {
      /* The err_str parameter to unix_error is supposed to be the function name
       * where the error occurred, but we insert the error description there
       * instead, which should be more useful.
       */
      unix_error(err_no, (char *) err_str, Nothing);

    } else {
      value exn;
      int error_to_raise = caml_zmq_EUNKNOWN;
      int i;
      for (i = 0; i < caml_zmq_EUNKNOWN; i++) {
          if (err_no == caml_zmq_error_table[i]) {
              error_to_raise = i;
              break;
          }
      }

      exn = caml_callback2(
          *caml_named_value("ZMQ.remap_exn"),
          Val_int(error_to_raise),
          caml_copy_string(err_str));
      caml_raise_constant(exn);
    }

    CAMLreturn0;
}

void caml_zmq_raise_if(int condition) {
    if (condition) {
        int err_no = zmq_errno();
        const char *err_str = zmq_strerror(err_no);
        caml_zmq_raise(err_no, err_str);
    }
}
