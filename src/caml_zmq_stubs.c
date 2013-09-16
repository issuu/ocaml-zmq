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
#endif


#include <zmq.h>

#include "fail.h"
#include "context.h"
#include "socket.h"

#include "uint64.h"

#define CAML_ZMQ_ARRAY_SIZE(a) (sizeof(a)/sizeof(a[0]))

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

CAMLprim value caml_zmq_init(value num_threads) {
    CAMLparam1 (num_threads);
    CAMLlocal1 (ctx_value);
    void *ctx = zmq_init(Int_val(num_threads));
    caml_zmq_raise_if(ctx == NULL);
    ctx_value = caml_zmq_copy_context(ctx);
    CAMLreturn (ctx_value);
}

/**
 * Term
 */

CAMLprim value caml_zmq_term(value ctx) {
    CAMLparam1 (ctx);
    int result = zmq_term(CAML_ZMQ_Context_val(ctx));
    caml_zmq_raise_if(result == -1);
    CAMLreturn (Val_unit);
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

    if (Int_val(socket_kind) < 0 || Int_val(socket_kind) > 8)
        caml_failwith("Invalid variant range");

    socket = zmq_socket(CAML_ZMQ_Context_val(ctx), socket_type_for_kind[Int_val(socket_kind)]);
    caml_zmq_raise_if(socket == NULL);
    sock_value = caml_zmq_copy_socket(socket);
    CAMLreturn (sock_value);
}

/**
 * Close
 */

CAMLprim value caml_zmq_close(value socket) {
    CAMLparam1 (socket);
    int result = zmq_close(CAML_ZMQ_Socket_val(socket));
    caml_zmq_raise_if(result == -1);
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

    uint64 val = CAML_UINT_Uint64_val(socket_option);
    int result = zmq_setsockopt(CAML_ZMQ_Socket_val(socket),
                                native_uint64_option_for[Int_val(option_name)],
                                &val,
                                sizeof(val));

    caml_zmq_raise_if (result == -1);

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

    caml_zmq_raise_if (result == -1);

    CAMLreturn (Val_unit);
}

static int const native_bytes_option_for[] = {
    ZMQ_IDENTITY,
    ZMQ_SUBSCRIBE,
    ZMQ_UNSUBSCRIBE,
    ZMQ_LAST_ENDPOINT,
    ZMQ_TCP_ACCEPT_FILTER
};


int caml_zmq_set_bytes_option(value socket, value option_name, value socket_option) {
    CAMLparam3 (socket, option_name, socket_option);

    char *option_value = String_val(socket_option);
    size_t option_size = caml_string_length(socket_option);
    int result = zmq_setsockopt(CAML_ZMQ_Socket_val(socket),
                                native_bytes_option_for[Int_val(option_name)],
                                option_value,
                                option_size);

    caml_zmq_raise_if (result == -1);

    CAMLreturn (Val_unit);
}

static int const native_int_option_for[] = {
    ZMQ_RATE,
    ZMQ_RECOVERY_IVL,
    ZMQ_SNDBUF,
    ZMQ_RCVBUF,
    ZMQ_RCVMORE,
    ZMQ_FD,
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
    ZMQ_IPV4ONLY,
    ZMQ_ROUTER_MANDATORY,
    ZMQ_TCP_KEEPALIVE,
    ZMQ_TCP_KEEPALIVE_CNT,
    ZMQ_TCP_KEEPALIVE_IDLE,
    ZMQ_TCP_KEEPALIVE_INTVL,
    ZMQ_DELAY_ATTACH_ON_CONNECT,
    ZMQ_XPUB_VERBOSE,
};

CAMLprim value caml_zmq_set_int_option(value socket, value option_name, value socket_option) {
    CAMLparam3 (socket, option_name, socket_option);

    int val = Int_val(socket_option);
    int result = zmq_setsockopt(CAML_ZMQ_Socket_val(socket),
                                native_int_option_for[Int_val(option_name)],
                                &val,
                                sizeof(val));

    caml_zmq_raise_if (result == -1);

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
    caml_zmq_raise_if (result == -1);
    CAMLreturn (caml_uint_copy_uint64(mark));
}

CAMLprim value caml_zmq_get_int64_option(value socket, value option_name) {
    CAMLparam2 (socket, option_name);
    int64 mark;
    size_t mark_size = sizeof (mark);
    int result = zmq_getsockopt (CAML_ZMQ_Socket_val(socket),
                                 native_int64_option_for[Int_val(option_name)],
                                 &mark,
                                 &mark_size);
    caml_zmq_raise_if (result == -1);
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
    caml_zmq_raise_if (result == -1);
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
    caml_zmq_raise_if (result == -1);
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
    caml_zmq_raise_if (result == -1);
    int event_type = 0; /* No_event */
    switch (event) {
        case ZMQ_POLLIN:
            event_type = 1;
            break;
        case ZMQ_POLLOUT:
            event_type = 2;
            break;
        case ZMQ_POLLIN | ZMQ_POLLOUT:
            event_type = 3;
            break;
        case ZMQ_POLLERR:
            event_type = 4;
            break;
        };
    CAMLreturn (Val_int(event_type));
}

CAMLprim value caml_zmq_get_fd(value socket) {
    CAMLparam1 (socket);
    #if defined(_WIN32) || defined(_WIN64)
    SOCKET fd;
    #else
    int fd;
    #endif
    size_t mark_size = sizeof (fd);
    int result = zmq_getsockopt (CAML_ZMQ_Socket_val(socket),
                                 ZMQ_FD,
                                 (void *)&fd,
                                 &mark_size);
    caml_zmq_raise_if (result == -1);
    #if defined(_WIN32) || defined(_WIN64)
    CAMLreturn (win_alloc_socket(fd));
    #else
    CAMLreturn (Val_int(fd));
    #endif
}



/**
 * Bind
 */

CAMLprim value caml_zmq_bind(value socket, value string_address) {
    CAMLparam2 (socket, string_address);
    int result = zmq_bind(CAML_ZMQ_Socket_val(socket), String_val(string_address));
    caml_zmq_raise_if (result == -1);
    CAMLreturn(Val_unit);
}

/**
 * Connect
 */

CAMLprim value caml_zmq_connect(value socket, value string_address) {
    CAMLparam2 (socket, string_address);
    int result = zmq_connect(CAML_ZMQ_Socket_val(socket), String_val(string_address));
    caml_zmq_raise_if (result == -1);
    CAMLreturn(Val_unit);
}

/**
 * Disconnect
 */
CAMLprim value caml_zmq_disconnect(value socket, value string_address) {
    CAMLparam2 (socket, string_address);
    int result = zmq_disconnect(CAML_ZMQ_Socket_val(socket), String_val(string_address));
    caml_zmq_raise_if (result == -1);
    CAMLreturn(Val_unit);
}


/**
 * Send
 */

/* Order must match Socket.snd_option declaration */
static int const native_snd_option_for_caml_snd_option[] = {
    0,
    ZMQ_NOBLOCK,
    ZMQ_SNDMORE,
    ZMQ_NOBLOCK | ZMQ_SNDMORE
};

static bool is_caml_snd_option_valid(int caml_snd_option) {
    return caml_snd_option > -1
           && caml_snd_option
              < (int) CAML_ZMQ_ARRAY_SIZE(native_snd_option_for_caml_snd_option);
}

CAMLprim value caml_zmq_send(value socket, value string, value snd_options) {
    CAMLparam3 (socket, string, snd_options);

    int caml_snd_option = Int_val(snd_options);
    if (!is_caml_snd_option_valid(caml_snd_option))
        caml_failwith("Invalid send option.");

    void *sock = CAML_ZMQ_Socket_val(socket);
    zmq_msg_t msg;
    int length = caml_string_length(string);
    int result = zmq_msg_init_size(&msg, length);
    caml_zmq_raise_if (result == -1);

    /* Doesn't copy '\0' */
    memcpy ((void *) zmq_msg_data (&msg), String_val(string), length);
    int option = native_snd_option_for_caml_snd_option[caml_snd_option];

    caml_release_runtime_system();
    result = zmq_msg_send(&msg, sock, option);
    caml_acquire_runtime_system();

    int close_result = zmq_msg_close (&msg);
    caml_zmq_raise_if (result == -1);
    caml_zmq_raise_if (close_result == -1);

    CAMLreturn(Val_unit);
}

/**
 * Receive
 */

/* Order must match Socket.recv_option declaration */
static int const native_rcv_option_for_caml_rcv_option[] = {
    0,
    ZMQ_NOBLOCK
};

static bool is_caml_rcv_option_valid(int caml_rcv_option) {
    return caml_rcv_option > -1
           && caml_rcv_option
              < (int) CAML_ZMQ_ARRAY_SIZE(native_rcv_option_for_caml_rcv_option);
}

CAMLprim value caml_zmq_recv(value socket, value rcv_options) {
    CAMLparam2 (socket, rcv_options);
    CAMLlocal1 (message);

    int caml_rcv_option = Int_val(rcv_options);
    if (!is_caml_rcv_option_valid(caml_rcv_option))
        caml_failwith("Invalid receive option.");

    void *sock = CAML_ZMQ_Socket_val(socket);
    int option = native_rcv_option_for_caml_rcv_option[caml_rcv_option];

    zmq_msg_t msg;
    int result = zmq_msg_init (&msg);
    caml_zmq_raise_if (result == -1);

    caml_release_runtime_system();
    result = zmq_msg_recv (&msg, sock, option);
    caml_acquire_runtime_system();

    caml_zmq_raise_if (result == -1);

    size_t size = zmq_msg_size (&msg);
    message = caml_alloc_string(size);
    memcpy (String_val(message), zmq_msg_data (&msg), size);
    result = zmq_msg_close(&msg);
    caml_zmq_raise_if (result == -1);
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
    caml_zmq_raise_if(result == -1);
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
    caml_zmq_raise_if(result == -1);
    CAMLreturn (Val_unit);
}

/**
 * Poll check poll.h
 */
