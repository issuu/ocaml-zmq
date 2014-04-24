/*
 * Copyright (C) 2011 Pedro Borges and contributors
 */

#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/intext.h>
#include <caml/threads.h>

#if defined(_WIN32) || defined(_WIN64)
#  include <winsock2.h>
#  include <windows.h>
#  define fd_type SOCKET
#  define Val_fd(x) win_alloc_socket(fd)
#else
#  define fd_type int
#  define Val_fd(x) Val_int(x)
#endif

#include <zmq.h>
#include <zmq_utils.h>

#include "fail.h"
#include "context.h"
#include "socket.h"

#include <uint64.h>

/**
 * Version
 */

CAMLprim value caml_zmq_version(value unit) {
    CAMLparam1 (unit);
    CAMLlocal1 (version_tuple);

    int major, minor, patch;
    zmq_version(&major, &minor, &patch);

    version_tuple = caml_alloc_tuple(3);
    Store_field(version_tuple, 0, Val_int(major));
    Store_field(version_tuple, 1, Val_int(minor));
    Store_field(version_tuple, 2, Val_int(patch));

    CAMLreturn (version_tuple);
}

/**
 * Init
 */

CAMLprim value caml_zmq_new(value num_threads) {
    CAMLparam1 (num_threads);
    CAMLlocal1 (ctx_value);

    void *ctx = zmq_ctx_new();
    caml_zmq_raise_if(ctx == NULL, "caml_zmq_new");

    ctx_value = caml_zmq_copy_context(ctx);
    CAMLreturn (ctx_value);
}

/**
 * Term
 */

CAMLprim value caml_zmq_term(value ctx) {
    CAMLparam1 (ctx);

    int result = zmq_ctx_term(CAML_ZMQ_Context_val(ctx));
    caml_zmq_raise_if(result == -1, "caml_zmq_term");

    CAML_ZMQ_Context_val(ctx) = NULL;
    CAMLreturn (Val_unit);
}

/**
 * Set context option
 */

static int const native_ctx_int_option_for[] = {
    ZMQ_IO_THREADS,
    ZMQ_MAX_SOCKETS,
    ZMQ_IPV6
};

CAMLprim value caml_zmq_ctx_set_int_option(value socket, value option_name, value option_value) {
    CAMLparam3 (socket, option_name, option_value);

    int result = zmq_ctx_set(CAML_ZMQ_Context_val(socket),
                             native_ctx_int_option_for[Int_val(option_name)],
                             Int_val(option_value));
    caml_zmq_raise_if (result == -1, "caml_zmq_ctx_set_int_option");

    CAMLreturn (Val_unit);
}

/**
 * Get context option
 */

CAMLprim value caml_zmq_ctx_get_int_option(value socket, value option_name) {
    CAMLparam2 (socket, option_name);

    int result = zmq_ctx_get(CAML_ZMQ_Context_val(socket),
                             native_ctx_int_option_for[Int_val(option_name)]);
    caml_zmq_raise_if (result == -1, "caml_zmq_ctx_get_int_option");

    CAMLreturn (Val_int(result));
}

/**
 * Socket
 */

/* Order must match OCaml's kind declaration */
static int const socket_type_for_kind[] =  {
    ZMQ_PAIR,
    ZMQ_PUB,
    ZMQ_SUB,
    ZMQ_REQ,
    ZMQ_REP,
    ZMQ_DEALER,
    ZMQ_ROUTER,
    ZMQ_PULL,
    ZMQ_PUSH,
    ZMQ_XPUB,
    ZMQ_XSUB
};

CAMLprim value caml_zmq_socket(value ctx, value socket_kind) {
    CAMLparam2 (ctx, socket_kind);
    CAMLlocal1 (sock_value);
    void *socket;

    socket = zmq_socket(CAML_ZMQ_Context_val(ctx), socket_type_for_kind[Int_val(socket_kind)]);
    caml_zmq_raise_if(socket == NULL, "caml_zmq_socket");
    sock_value = caml_zmq_copy_socket(socket);
    CAMLreturn (sock_value);
}

/**
 * Close
 */

CAMLprim value caml_zmq_close(value socket) {
    CAMLparam1 (socket);
    int result = zmq_close(CAML_ZMQ_Socket_val(socket));
    caml_zmq_raise_if(result == -1, "caml_zmq_close");
    CAMLreturn (Val_unit);
}

/**
 * Set socket options
 */

static int const native_uint64_option_for[] = {
    ZMQ_AFFINITY
};

CAMLprim value caml_zmq_set_uint64_option(value socket, value option_name, value socket_option) {
    CAMLparam3 (socket, option_name, socket_option);

    uint64 val = Uint64_val(socket_option);
    int result = zmq_setsockopt(CAML_ZMQ_Socket_val(socket),
                                native_uint64_option_for[Int_val(option_name)],
                                &val,
                                sizeof(val));

    caml_zmq_raise_if (result == -1, "caml_zmq_set_uint64_option");

    CAMLreturn (Val_unit);
}

static int const native_int64_option_for[] = {
    ZMQ_MAXMSGSIZE
};

CAMLprim value caml_zmq_set_int64_option(value socket, value option_name, value socket_option) {
    CAMLparam3 (socket, option_name, socket_option);

    int64 val = Int64_val(Field(socket_option, 1));
    int result = zmq_setsockopt(CAML_ZMQ_Socket_val(socket),
                                native_int64_option_for[Int_val(option_name)],
                                &val,
                                sizeof(val));

    caml_zmq_raise_if (result == -1, "caml_zmq_set_int64_option");

    CAMLreturn (Val_unit);
}

static int const native_bytes_option_for[] = {
    ZMQ_IDENTITY,
    ZMQ_SUBSCRIBE,
    ZMQ_UNSUBSCRIBE,
    ZMQ_LAST_ENDPOINT,
    ZMQ_TCP_ACCEPT_FILTER,
    ZMQ_PLAIN_USERNAME,
    ZMQ_PLAIN_PASSWORD,
    ZMQ_CURVE_PUBLICKEY,
    ZMQ_CURVE_SECRETKEY,
    ZMQ_CURVE_SERVERKEY,
    ZMQ_ZAP_DOMAIN,
};

int caml_zmq_set_bytes_option(value socket, value option_name, value socket_option) {
    CAMLparam3 (socket, option_name, socket_option);

    char *option_value = String_val(socket_option);
    size_t option_size = caml_string_length(socket_option);
    int result = zmq_setsockopt(CAML_ZMQ_Socket_val(socket),
                                native_bytes_option_for[Int_val(option_name)],
                                option_value,
                                option_size);

    caml_zmq_raise_if (result == -1, "caml_zmq_set_bytes_option");

    CAMLreturn (Val_unit);
}

static int const native_int_option_for[] = {
    ZMQ_RATE,
    ZMQ_RECOVERY_IVL,
    ZMQ_SNDBUF,
    ZMQ_RCVBUF,
    ZMQ_RCVMORE,
    ZMQ_EVENTS,
    ZMQ_TYPE,
    ZMQ_LINGER,
    ZMQ_RECONNECT_IVL,
    ZMQ_BACKLOG,
    ZMQ_RECONNECT_IVL_MAX,
    ZMQ_SNDHWM,
    ZMQ_RCVHWM,
    ZMQ_MULTICAST_HOPS,
    ZMQ_RCVTIMEO,
    ZMQ_SNDTIMEO,
    ZMQ_IPV6,
    ZMQ_ROUTER_MANDATORY,
    ZMQ_TCP_KEEPALIVE,
    ZMQ_TCP_KEEPALIVE_CNT,
    ZMQ_TCP_KEEPALIVE_IDLE,
    ZMQ_TCP_KEEPALIVE_INTVL,
    ZMQ_IMMEDIATE,
    ZMQ_XPUB_VERBOSE,
    ZMQ_MECHANISM,
    ZMQ_PLAIN_SERVER,
    ZMQ_CURVE_SERVER,
    ZMQ_PROBE_ROUTER,
    ZMQ_REQ_CORRELATE,
    ZMQ_REQ_RELAXED,
    ZMQ_CONFLATE,
};

CAMLprim value caml_zmq_set_int_option(value socket, value option_name, value socket_option) {
    CAMLparam3 (socket, option_name, socket_option);

    int val = Int_val(socket_option);
    int result = zmq_setsockopt(CAML_ZMQ_Socket_val(socket),
                                native_int_option_for[Int_val(option_name)],
                                &val,
                                sizeof(val));

    caml_zmq_raise_if (result == -1, "caml_zmq_set_int_option");

    CAMLreturn (Val_unit);
}

/**
 * Get socket options
 */

CAMLprim value caml_zmq_get_uint64_option(value socket, value option_name) {
    CAMLparam2 (socket, option_name);
    uint64 mark;
    size_t mark_size = sizeof (mark);
    int result = zmq_getsockopt (CAML_ZMQ_Socket_val(socket),
                                 native_uint64_option_for[Int_val(option_name)],
                                 &mark,
                                 &mark_size);
    caml_zmq_raise_if (result == -1, "caml_zmq_get_uint64_option");
    CAMLreturn (copy_uint64(mark));
}

CAMLprim value caml_zmq_get_int64_option(value socket, value option_name) {
    CAMLparam2 (socket, option_name);
    int64 mark;
    size_t mark_size = sizeof (mark);
    int result = zmq_getsockopt (CAML_ZMQ_Socket_val(socket),
                                 native_int64_option_for[Int_val(option_name)],
                                 &mark,
                                 &mark_size);
    caml_zmq_raise_if (result == -1, "caml_zmq_get_int64_option");
    CAMLreturn (caml_copy_int64(mark));
}

CAMLprim value caml_zmq_get_bytes_option(value socket, value option_name) {
    CAMLparam2 (socket, option_name);
    char buffer[256];
    size_t buffer_size = sizeof (buffer);
    int result = zmq_getsockopt (CAML_ZMQ_Socket_val(socket),
                                 native_bytes_option_for[Int_val(option_name)],
                                 buffer,
                                 &buffer_size);
    buffer[result] = '\0';
    caml_zmq_raise_if (result == -1, "caml_zmq_get_bytes_option");
    CAMLreturn (caml_copy_string(buffer));
}

CAMLprim value caml_zmq_get_int_option(value socket, value option_name) {
    CAMLparam2 (socket, option_name);
    int mark;
    size_t mark_size = sizeof (mark);
    int result = zmq_getsockopt (CAML_ZMQ_Socket_val(socket),
                                 native_int_option_for[Int_val(option_name)],
                                 &mark,
                                 &mark_size);
    caml_zmq_raise_if (result == -1, "caml_zmq_get_int_option");
    CAMLreturn (Val_int(mark));
}

CAMLprim value caml_zmq_get_events(value socket) {
    CAMLparam1 (socket);
    uint32_t event = 0;
    size_t event_size = sizeof (event);
    int result = zmq_getsockopt (CAML_ZMQ_Socket_val(socket),
                                 ZMQ_EVENTS,
                                 &event,
                                 &event_size);
    caml_zmq_raise_if (result == -1, "caml_zmq_get_events");
    int event_type = 0; /* No_event */
    if (event & ZMQ_POLLIN) {
        event_type = 1; /* Poll_in */
        if (event & ZMQ_POLLOUT) {
            event_type = 3; /* Poll_in_out */
        }
    } else if (event & ZMQ_POLLOUT) {
        event_type = 2; /* Poll_out */
    } else if (event & ZMQ_POLLERR) {
        event_type = 4; /* Poll_error */
    }
    CAMLreturn (Val_int(event_type));
}

CAMLprim value caml_zmq_get_fd(value socket) {
    CAMLparam1 (socket);
    fd_type fd;
    size_t mark_size = sizeof (fd);
    int result = zmq_getsockopt (CAML_ZMQ_Socket_val(socket),
                                 ZMQ_FD,
                                 (void *)&fd,
                                 &mark_size);
    caml_zmq_raise_if (result == -1, "caml_zmq_get_fd");
    CAMLreturn (Val_fd(fd));
}



/**
 * Bind
 */

CAMLprim value caml_zmq_bind(value socket, value string_address) {
    CAMLparam2 (socket, string_address);
    int result = zmq_bind(CAML_ZMQ_Socket_val(socket), String_val(string_address));
    caml_zmq_raise_if (result == -1, "caml_zmq_bind");
    CAMLreturn(Val_unit);
}

/**
 * Connect
 */

CAMLprim value caml_zmq_connect(value socket, value string_address) {
    CAMLparam2 (socket, string_address);
    int result = zmq_connect(CAML_ZMQ_Socket_val(socket), String_val(string_address));
    caml_zmq_raise_if (result == -1, "caml_zmq_connect");
    CAMLreturn(Val_unit);
}

/**
 * Disconnect
 */
CAMLprim value caml_zmq_disconnect(value socket, value string_address) {
    CAMLparam2 (socket, string_address);
    int result = zmq_disconnect(CAML_ZMQ_Socket_val(socket), String_val(string_address));
    caml_zmq_raise_if (result == -1, "caml_zmq_disconnect");
    CAMLreturn(Val_unit);
}


/**
 * Send
 */
CAMLprim value caml_zmq_send(value socket, value string, value block_flag, value more_flag) {
    CAMLparam4 (socket, string, block_flag, more_flag);

    int option = 0;
    if (! Bool_val(block_flag)) option |= ZMQ_NOBLOCK;
    if (Bool_val(more_flag)) option |= ZMQ_SNDMORE;

    void *sock = CAML_ZMQ_Socket_val(socket);
    zmq_msg_t msg;
    int length = caml_string_length(string);
    int result = zmq_msg_init_size(&msg, length);
    caml_zmq_raise_if (result == -1, "caml_zmq_send");

    /* Doesn't copy '\0' */
    memcpy ((void *) zmq_msg_data (&msg), String_val(string), length);

    caml_release_runtime_system();
    result = zmq_msg_send(&msg, sock, option);
    caml_acquire_runtime_system();

    int close_result = zmq_msg_close (&msg);
    caml_zmq_raise_if (result == -1, "caml_zmq_send");
    caml_zmq_raise_if (close_result == -1, "caml_zmq_send");

    CAMLreturn(Val_unit);
}

/**
 * Receive
 */

CAMLprim value caml_zmq_recv(value socket, value block_flag) {
    CAMLparam2 (socket, block_flag);
    CAMLlocal1 (message);

    int option = 0;
    if (!Bool_val(block_flag)) option |= ZMQ_NOBLOCK;

    void *sock = CAML_ZMQ_Socket_val(socket);

    zmq_msg_t msg;
    int result = zmq_msg_init (&msg);
    caml_zmq_raise_if (result == -1, "caml_zmq_recv");

    caml_release_runtime_system();
    result = zmq_msg_recv (&msg, sock, option);
    caml_acquire_runtime_system();

    caml_zmq_raise_if (result == -1, "caml_zmq_recv");

    size_t size = zmq_msg_size (&msg);
    message = caml_alloc_string(size);
    memcpy (String_val(message), zmq_msg_data (&msg), size);
    result = zmq_msg_close(&msg);
    caml_zmq_raise_if (result == -1, "caml_zmq_recv");
    CAMLreturn (message);
}

/**
 * Devices
 */

CAMLprim value caml_zmq_proxy2(value frontend, value backend) {
    CAMLparam2 (frontend, backend);

    void *native_frontend = CAML_ZMQ_Socket_val(frontend);
    void *native_backend = CAML_ZMQ_Socket_val(backend);

    caml_release_runtime_system();
    int result = zmq_proxy(native_frontend, native_backend, NULL);
    caml_acquire_runtime_system();

    /* Will awlays raise an exception */
    caml_zmq_raise_if(result == -1, "caml_zmq_proxy2");
    CAMLreturn (Val_unit);
}

CAMLprim value caml_zmq_proxy3(value frontend, value backend, value capture) {
    CAMLparam3 (frontend, backend, capture);

    void *native_frontend = CAML_ZMQ_Socket_val(frontend);
    void *native_backend = CAML_ZMQ_Socket_val(backend);
    void *native_capture = CAML_ZMQ_Socket_val(capture);

    caml_release_runtime_system();
    int result = zmq_proxy(native_frontend, native_backend, native_capture);
    caml_acquire_runtime_system();

    /* Will awlays raise an exception */
    caml_zmq_raise_if(result == -1, "caml_zmq_proxy3");
    CAMLreturn (Val_unit);
}

CAMLprim value caml_zmq_socket_monitor(value socket, value address) {
    CAMLparam2 (socket, address);

    caml_release_runtime_system();
    int result = zmq_socket_monitor(CAML_ZMQ_Socket_val(socket), String_val(address), ZMQ_EVENT_ALL);
    caml_acquire_runtime_system();

    /* Will awlays raise an exception */
    caml_zmq_raise_if(result == -1, "caml_zmq_socket_monitor");
    CAMLreturn (Val_unit);
}

enum event_type {
    CONNECTED = 0,
    CONNECT_DELAYED,
    CONNECT_RETRIED,
    LISTENING,
    BIND_FAILED,
    ACCEPTED,
    ACCEPT_FAILED,
    CLOSED,
    CLOSE_FAILED,
    DISCONNECTED,
};

/** Decode monitor event */
CAMLprim value caml_decode_monitor_event(value event_val, value addr) {
    CAMLparam2 (event_val, addr);
    CAMLlocal1 (result);

    const char *data = String_val(event_val);
    zmq_event_t event;
    /* Dont just map the event structure, as the struct is not packed. */
    memcpy(&(event.event), data, sizeof(event.event));
    memcpy(&(event.value), data + sizeof(event.event), sizeof(event.value));

    switch (event.event) {
    case ZMQ_EVENT_CONNECTED:
        result = caml_alloc(2, CONNECTED);
        Store_field(result, 0, addr);
        Store_field(result, 1, Val_fd(event.value));
        break;

    case ZMQ_EVENT_CONNECT_DELAYED:
        result = caml_alloc(1, CONNECT_DELAYED);
        Store_field(result, 0, addr);
        break;

    case ZMQ_EVENT_CONNECT_RETRIED:
        result = caml_alloc(2, CONNECT_RETRIED);
        Store_field(result, 0, addr);
        Store_field(result, 1, Val_int(event.value));
        break;

    case ZMQ_EVENT_LISTENING:
        result = caml_alloc(2, LISTENING);
        Store_field(result, 0, addr);
        Store_field(result, 1, Val_fd(event.value));
        break;

    case ZMQ_EVENT_BIND_FAILED:
        result = caml_alloc(2, BIND_FAILED);
        Store_field(result, 0, addr);
        Store_field(result, 1, Val_int(event.value));
        Store_field(result, 2, caml_copy_string(zmq_strerror(event.value)));
        break;

    case ZMQ_EVENT_ACCEPTED:
        result = caml_alloc(2, ACCEPTED);
        Store_field(result, 0, addr);
        Store_field(result, 1, Val_fd(event.value));
        break;

    case ZMQ_EVENT_ACCEPT_FAILED:
        result = caml_alloc(2, ACCEPT_FAILED);
        Store_field(result, 0, addr);
        Store_field(result, 1, Val_int(event.value));
        Store_field(result, 2, caml_copy_string(zmq_strerror(event.value)));
        break;

    case ZMQ_EVENT_CLOSED:
        result = caml_alloc(2, CLOSED);
        Store_field(result, 0, addr);
        Store_field(result, 1, Val_fd(event.value));
        break;

    case ZMQ_EVENT_CLOSE_FAILED:
        result = caml_alloc(2, CLOSE_FAILED);
        Store_field(result, 0, addr);
        Store_field(result, 1, Val_int(event.value));
        Store_field(result, 2, caml_copy_string(zmq_strerror(event.value)));
        break;
    case ZMQ_EVENT_DISCONNECTED:
        result = caml_alloc(2, DISCONNECTED);
        Store_field(result, 0, addr);
        Store_field(result, 1, Val_fd(event.value));
        break;
    default:
        caml_zmq_raise(EFAULT, "caml_decode_monitor_event");
        break;
    }
    CAMLreturn(result);
}

/**
 * Z85
 */

CAMLprim value caml_z85_encode(value source) {
    CAMLparam1 (source);
    CAMLlocal1 (result);

    /*
     * zmq_z85_encode writes a null terminator. However, OCaml does not
     * need a null terminator. The code below does not allocate space for
     * a null terminator, but zmq_z85_encode will not encounter an overrun,
     * as OCaml string representation guarantees that one byte past the
     * end of string is allocated and contains '\0'.
     *
     * See http://caml.inria.fr/pub/docs/oreilly-book/html/book-ora115.html#@concepts266
     * for details.
     */
    int length = caml_string_length(source);
    result = caml_alloc_string(length / 4 * 5);
    if (zmq_z85_encode(String_val(result), (uint8_t*) String_val(source), length) == NULL)
        caml_invalid_argument("zmq_z85_encode");

    CAMLreturn(result);
}

CAMLprim value caml_z85_decode(value source) {
    CAMLparam1 (source);
    CAMLlocal1 (result);

    result = caml_alloc_string(caml_string_length(source) * 4 / 5);
    if (zmq_z85_decode((uint8_t*) String_val(result), String_val(source)) == NULL)
        caml_invalid_argument("zmq_z85_decode");

    CAMLreturn(result);
}

/**
 * Key generation
 */

CAMLprim value caml_curve_keypair(value unit) {
    CAMLparam1 (unit);
    CAMLlocal3 (public, secret, tuple);

    /* See the notice in caml_z85_encode. */
    public = caml_alloc_string(40);
    secret = caml_alloc_string(40);
    int result = zmq_curve_keypair(String_val(public), String_val(secret));
    caml_zmq_raise_if(result == -1, "caml_curve_keypair");

    tuple = caml_alloc_tuple(2);
    Store_field(tuple, 0, public);
    Store_field(tuple, 1, secret);
    CAMLreturn (tuple);
}
