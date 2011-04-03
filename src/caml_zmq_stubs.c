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
    ZMQ_PUSH
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
    ZMQ_HWM,
    ZMQ_AFFINITY,
    ZMQ_SNDBUF,
    ZMQ_RCVBUF
};

CAMLprim value caml_zmq_set_uint64_option(value socket, value option_name, value socket_option) {
    CAMLparam3 (socket, option_name, socket_option);

    uint64 mark = CAML_UINT_Uint64_val(socket_option);
    void *option_value = (void *) &mark;
    size_t option_size = sizeof (mark);
    int result = zmq_setsockopt(CAML_ZMQ_Socket_val(socket),
                                native_uint64_option_for[Int_val(option_name)],
                                option_value,
                                option_size);
    
    caml_zmq_raise_if (result == -1);

    CAMLreturn (Val_unit);
}

static int const native_int64_option_for[] = {
    ZMQ_SWAP,
    ZMQ_RATE,
    ZMQ_RECOVERY_IVL,
    ZMQ_RECOVERY_IVL_MSEC,
    ZMQ_MCAST_LOOP,
    ZMQ_RCVMORE
};

CAMLprim value caml_zmq_set_int64_option(value socket, value option_name, value socket_option) {
    CAMLparam3 (socket, option_name, socket_option);

    int64 mark = Int64_val(Field(socket_option, 1));
    void *option_value = (void *) &mark;
    size_t option_size = sizeof (mark);
    int result = zmq_setsockopt(CAML_ZMQ_Socket_val(socket),
                                native_int64_option_for[Int_val(option_name)],
                                option_value,
                                option_size);

    caml_zmq_raise_if (result == -1);

    CAMLreturn (Val_unit);
}

static int const native_bytes_option_for[] = {
    ZMQ_IDENTITY,
    ZMQ_SUBSCRIBE,
    ZMQ_UNSUBSCRIBE
};


int caml_zmq_set_bytes_option(value socket, value option_name, value socket_option) {
    CAMLparam3 (socket, option_name, socket_option);

    char *unsubs = String_val(socket_option);
    void *option_value = (void *) unsubs;
    size_t option_size = caml_string_length(socket_option);
    int result = zmq_setsockopt(CAML_ZMQ_Socket_val(socket),
                                native_bytes_option_for[Int_val(option_name)],
                                option_value,
                                option_size);

    caml_zmq_raise_if (result == -1);

    CAMLreturn (Val_unit);   
}

static int const native_int_option_for[] = {
    ZMQ_LINGER,
    ZMQ_RECONNECT_IVL,
    ZMQ_RECONNECT_IVL_MAX,
    ZMQ_BACKLOG
};

CAMLprim value caml_zmq_set_int_option(value socket, value option_name, value socket_option) {
    CAMLparam3 (socket, option_name, socket_option);

    int mark = Int_val(socket_option);
    void *option_value = (void *) &mark;
    size_t option_size = sizeof (mark);
    int result = zmq_setsockopt(CAML_ZMQ_Socket_val(socket),
                                native_int_option_for[Int_val(option_name)],
                                option_value,
                                option_size);

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
    if (event & ZMQ_POLLIN) {
        event_type = 1; /* Poll_in */
        if (event & ZMQ_POLLOUT) {
            event_type = 3; /* Poll_in_out */
        }
    } else if (event & ZMQ_POLLOUT) {
        event_type = 2; /* Poll_out */
    }
    CAMLreturn (Val_int(event));
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
 * Send
 */

/* Order must match Socket.snd_option declaration */
static int const native_snd_option_for_caml_snd_option[] = {
    0,
    ZMQ_NOBLOCK,
    ZMQ_SNDMORE
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
    int result = zmq_msg_init_size(&msg, caml_string_length(string));
    caml_zmq_raise_if (result == -1);

    /* Doesn't copy '\0' */
    memcpy ((void *) zmq_msg_data (&msg), String_val(string), caml_string_length(string));
    int option = native_snd_option_for_caml_snd_option[caml_snd_option];

    caml_release_runtime_system();
    result = zmq_send (sock, &msg, option);
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

    zmq_msg_t request;
    int result = zmq_msg_init (&request);
    caml_zmq_raise_if (result == -1);

    caml_release_runtime_system();
    result = zmq_recv (sock, &request, option);
    caml_acquire_runtime_system();

    caml_zmq_raise_if (result == -1);

    size_t size = zmq_msg_size (&request);
    message = caml_alloc_string(size);
    memcpy (String_val(message), zmq_msg_data (&request), size);
    result = zmq_msg_close(&request);
    caml_zmq_raise_if (result == -1);
    CAMLreturn (message);
}

/**
 * Devices
 */

/* Order must match Device.kind declaration */
static int const native_device_for_caml_device[] = {
    ZMQ_STREAMER,
    ZMQ_FORWARDER,
    ZMQ_QUEUE
};

static bool is_caml_device_valid(int caml_device) {
    return caml_device > -1
           && caml_device < (int) CAML_ZMQ_ARRAY_SIZE(native_device_for_caml_device);
}

CAMLprim value caml_zmq_device(value device_kind, value socket1, value socket2) {
    CAMLparam3 (device_kind, socket1, socket2);
    int caml_device = Int_val(device_kind);

    if (!is_caml_device_valid(caml_device))
        caml_failwith("Invalid device kind");
	
    void *native_socket1 = CAML_ZMQ_Socket_val(socket1);
    void *native_socket2 = CAML_ZMQ_Socket_val(socket2);

    caml_release_runtime_system();  
    int result = zmq_device(native_device_for_caml_device[caml_device],
                            native_socket1,
			    native_socket2);
    caml_acquire_runtime_system();

    caml_zmq_raise_if(result == -1);
    CAMLreturn (Val_unit);
}

/**
 * Poll check poll.h
 */

