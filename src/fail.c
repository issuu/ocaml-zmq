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

/* This table must be synchronized with the variant definition. */
static int const caml_zmq_error_table[] = {
    EFSM,
    ENOCOMPATPROTO,
    ETERM,
    EMTHREAD,
};

/* This must be the last value of the variant. */
static int const caml_zmq_EUNKNOWN =
    (sizeof caml_zmq_error_table) / (sizeof caml_zmq_error_table[0]);

enum _OcamlErrorConstructors {
  /* copied from otherlibs/unix/unixsupport.c and prepended with c_ 
     maps error codes to constructor indices.
     Must be kept in sync with unix_support.c error_table. */
  c_E2BIG, c_EACCES, c_EAGAIN, c_EBADF, c_EBUSY, c_ECHILD, c_EDEADLK, c_EDOM,
  c_EEXIST, c_EFAULT, c_EFBIG, c_EINTR, c_EINVAL, c_EIO, c_EISDIR, c_EMFILE, c_EMLINK,
  c_ENAMETOOLONG, c_ENFILE, c_ENODEV, c_ENOENT, c_ENOEXEC, c_ENOLCK, c_ENOMEM, c_ENOSPC,
  c_ENOSYS, c_ENOTDIR, c_ENOTEMPTY, c_ENOTTY, c_ENXIO, c_EPERM, c_EPIPE, c_ERANGE,
  c_EROFS, c_ESPIPE, c_ESRCH, c_EXDEV, c_EWOULDBLOCK, c_EINPROGRESS, c_EALREADY,
  c_ENOTSOCK, c_EDESTADDRREQ, c_EMSGSIZE, c_EPROTOTYPE, c_ENOPROTOOPT,
  c_EPROTONOSUPPORT, c_ESOCKTNOSUPPORT, c_EOPNOTSUPP, c_EPFNOSUPPORT,
  c_EAFNOSUPPORT, c_EADDRINUSE, c_EADDRNOTAVAIL, c_ENETDOWN, c_ENETUNREACH,
  c_ENETRESET, c_ECONNABORTED, c_ECONNRESET, c_ENOBUFS, c_EISCONN, c_ENOTCONN,
  c_ESHUTDOWN, c_ETOOMANYREFS, c_ETIMEDOUT, c_ECONNREFUSED, c_EHOSTDOWN,
  c_EHOSTUNREACH, c_ELOOP, c_EOVERFLOW /*, c_EUNKNOWNERR */
};

static value * unix_error_exn = NULL;

static void caml_zmq_unix_error_constr(int constr_no, const char *cmdname, value cmdarg) {
  value res;
  value name = Val_unit, err = Val_unit, arg = Val_unit;

  Begin_roots3 (name, err, arg);
    arg = cmdarg == Nothing ? copy_string("") : cmdarg;
    name = copy_string(cmdname);
    err = Val_int(constr_no);
    if (unix_error_exn == NULL) {
      unix_error_exn = caml_named_value("Unix.Unix_error");
      if (unix_error_exn == NULL)
        invalid_argument("Exception Unix.Unix_error not initialized,"
                         " please link unix.cma");
    }
    res = alloc_small(4, 0);
    Field(res, 0) = *unix_error_exn;
    Field(res, 1) = err;
    Field(res, 2) = name;
    Field(res, 3) = arg;
  End_roots();
  mlraise(res);
}

void caml_zmq_raise(int err_no, const char *err_path) {
    CAMLparam0 ();
    CAMLlocalN(error_parameters, 2);

    if (err_no < ZMQ_HAUSNUMERO) {
      
      /* Map to Unix_error */
      unix_error(err_no, (char *) err_path, Nothing);
    
    } else {

      /* (re-)defined by ZMQ depending on platform, map to Unix_error */
      switch (err_no) {
      case ENOTSUP:
        caml_zmq_unix_error_constr(c_EOPNOTSUPP, err_path, Nothing);
        break;
      case EPROTONOSUPPORT:
        caml_zmq_unix_error_constr(c_EPROTONOSUPPORT, err_path, Nothing);
        break;
      case ENOBUFS:
        caml_zmq_unix_error_constr(c_ENOBUFS, err_path, Nothing);
        break;
      case ENETDOWN:
        caml_zmq_unix_error_constr(c_ENETDOWN, err_path, Nothing);
        break;
      case EADDRINUSE:
        caml_zmq_unix_error_constr(c_EADDRINUSE, err_path, Nothing);
        break;
      case EADDRNOTAVAIL:
        caml_zmq_unix_error_constr(c_EADDRNOTAVAIL, err_path, Nothing);
        break;
      case ECONNREFUSED:
        caml_zmq_unix_error_constr(c_ECONNREFUSED, err_path, Nothing);
        break;
      case EINPROGRESS:
        caml_zmq_unix_error_constr(c_EINPROGRESS, err_path, Nothing);
        break;
      case ENOTSOCK:
        caml_zmq_unix_error_constr(c_ENOTSOCK, err_path, Nothing);
        break;
      case EMSGSIZE:
        caml_zmq_unix_error_constr(c_EMSGSIZE, err_path, Nothing);
        break;
      case EAFNOSUPPORT:
        caml_zmq_unix_error_constr(c_EAFNOSUPPORT, err_path, Nothing);
        break;
      case ENETUNREACH:
        caml_zmq_unix_error_constr(c_ENETUNREACH, err_path, Nothing);
        break;
      case ECONNABORTED:
        caml_zmq_unix_error_constr(c_ECONNABORTED, err_path, Nothing);
        break;
      case ECONNRESET:
        caml_zmq_unix_error_constr(c_ECONNRESET, err_path, Nothing);
        break;
      case ENOTCONN:
        caml_zmq_unix_error_constr(c_ENOTCONN, err_path, Nothing);
        break;
      case ETIMEDOUT:
        caml_zmq_unix_error_constr(c_ETIMEDOUT, err_path, Nothing);
        break;
      case EHOSTUNREACH:
        caml_zmq_unix_error_constr(c_EHOSTUNREACH, err_path, Nothing);
        break;
      case ENETRESET:
        caml_zmq_unix_error_constr(c_ENETRESET, err_path, Nothing);
        break;
      default:
        /* ZMQ error */
        {
          int error_to_raise = caml_zmq_EUNKNOWN;
          int i;
          const char *err_str = zmq_strerror(err_no);
          for (i = 0; i < caml_zmq_EUNKNOWN; i++) {
              if (err_no == caml_zmq_error_table[i]) {
                  error_to_raise = i;
                  break;
              }
          }
          error_parameters[0] = Val_int(error_to_raise);
          error_parameters[1] = caml_copy_string(err_str);
          caml_raise_with_args(*caml_named_value("ZMQ.ZMQ_exception"),
                              2, error_parameters);
        }
      }

    }

    CAMLreturn0;
}

void caml_zmq_raise_if(int condition, char *err_path) {
    if (condition) {
        int err_no = zmq_errno();
        caml_zmq_raise(err_no, err_path);
    }
}

