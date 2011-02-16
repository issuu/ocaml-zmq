/*
 * Copyright (C) 2011 Pedro Borges and contributors
 */

#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/intext.h>
#include <caml/threads.h>

#include <zmq.h>

#include "caml_zmq/fail.h"
#include "caml_zmq/context.h"
#include "caml_zmq/socket.h"

#include "caml_uint/uint64.h"

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

CAMLprim value caml_zmq_init(value num_threads) {
    CAMLparam1 (num_threads);
    CAMLlocal1 (ctx_value);
    void *ctx = zmq_init(Int_val(num_threads));
    caml_zmq_raise_if(ctx == NULL);
    ctx_value = caml_zmq_copy_context(ctx);
    CAMLreturn (ctx_value);
}

CAMLprim value caml_zmq_term(value ctx) {
    CAMLparam1 (ctx);
    int result = zmq_term(CAML_ZMQ_Context_val(ctx));
    caml_zmq_raise_if(result == -1);
    CAMLreturn (Val_unit);
}

/* Order must match OCaml's variant declaration */
static int socket_type_for_kind[] =  {
    ZMQ_PAIR,
    ZMQ_PUB,
    ZMQ_SUB,
    ZMQ_REQ,
    ZMQ_REP,
    ZMQ_XREQ,
    ZMQ_XREP,
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

CAMLprim value caml_zmq_close(value socket) {
    CAMLparam1 (socket);
    int result = zmq_close(CAML_ZMQ_Socket_val(socket));
    caml_zmq_raise_if(result == -1);
    CAMLreturn (Val_unit);
}

int caml_zmq_setsockopt_uint64(value socket, int option_name, value socket_option) {
    CAMLparam2 (socket, socket_option);
    uint64 mark = CAML_UINT_Uint64_val(Field(socket_option, 1));
    void *option_value = (void *) &mark;
    size_t option_size = sizeof (mark);
    CAMLreturnT(int, zmq_setsockopt(CAML_ZMQ_Socket_val(socket),
                                    option_name,
                                    option_value,
                                    option_size));
}

int caml_zmq_setsockopt_int64(value socket, int option_name, value socket_option) {
    CAMLparam2 (socket, socket_option);
    int64 mark = Int64_val(Field(socket_option, 1));
    void *option_value = (void *) &mark;
    size_t option_size = sizeof (mark);
    CAMLreturnT(int, zmq_setsockopt(CAML_ZMQ_Socket_val(socket),
                                    option_name,
                                    option_value,
                                    option_size));
}

int caml_zmq_setsockopt_bytes(value socket, int option_name, value socket_option) {
    CAMLparam2 (socket, socket_option);
    char *unsubs = String_val(Field(socket_option, 1));
    void *option_value = (void *) unsubs;
    size_t option_size = strlen(unsubs);
    CAMLreturnT(int, zmq_setsockopt(CAML_ZMQ_Socket_val(socket),
                                    option_name,
                                    option_value,
                                    option_size));
}

struct caml_zmq_set_dispacher_row {
    char *option_id;
    int option_name;
    int (*option)(value, int, value);
};

static struct caml_zmq_set_dispacher_row const caml_zmq_set_dispacher[] = {
    { "High_water_mark"  , ZMQ_HWM         , &caml_zmq_setsockopt_uint64 },
    { "Swap"             , ZMQ_SWAP        , &caml_zmq_setsockopt_int64  },
    { "Affinity"         , ZMQ_AFFINITY    , &caml_zmq_setsockopt_uint64 },
    { "Identity"         , ZMQ_IDENTITY    , &caml_zmq_setsockopt_bytes  },
    { "Subscribe"        , ZMQ_SUBSCRIBE   , &caml_zmq_setsockopt_bytes  },
    { "Unsubscribe"      , ZMQ_UNSUBSCRIBE , &caml_zmq_setsockopt_bytes  },
    { "Rate"             , ZMQ_RATE        , &caml_zmq_setsockopt_int64  },
    { "Recovery_interval", ZMQ_RECOVERY_IVL, &caml_zmq_setsockopt_int64  },
    { "Multicast_loop"   , ZMQ_MCAST_LOOP  , &caml_zmq_setsockopt_int64  },
    { "Send_buffer"      , ZMQ_SNDBUF      , &caml_zmq_setsockopt_uint64 },
    { "Recieve_buffer"   , ZMQ_RCVBUF      , &caml_zmq_setsockopt_uint64 },
    { NULL               , 0               , NULL                        }
};

CAMLprim value caml_zmq_setsockopt(value socket, value sockopt) {
    CAMLparam2 (socket, sockopt);
    CAMLlocal1 (variant_hash);
    int result;
    int i;
    variant_hash = Field(sockopt, 0);
    for (i = 0; caml_zmq_set_dispacher[i].option_id != NULL; i++) {
        if(caml_hash_variant(caml_zmq_set_dispacher[i].option_id) == variant_hash) {
            result = caml_zmq_set_dispacher[i].option(socket, caml_zmq_set_dispacher[i].option_name, sockopt);
            break;
        }
    }
    if(caml_zmq_set_dispacher[i].option_id == NULL) {
        caml_failwith("Invalid option");
    }
    caml_zmq_raise_if(result == -1);
    CAMLreturn (Val_unit);
}

value caml_zmq_getsockopt_uint64(value socket, int option_name) {
    CAMLparam1 (socket);
    uint64 mark;
    size_t mark_size = sizeof (mark);
    int result = zmq_getsockopt (CAML_ZMQ_Socket_val(socket), option_name, &mark, &mark_size);
    caml_zmq_raise_if (result == -1);
    CAMLreturn (caml_uint_copy_uint64(mark));
}

value caml_zmq_getsockopt_int64(value socket, int option_name) {
    CAMLparam1 (socket);
    int64 mark;
    size_t mark_size = sizeof (mark);
    int result = zmq_getsockopt (CAML_ZMQ_Socket_val(socket), option_name, &mark, &mark_size);
    caml_zmq_raise_if (result == -1);
    CAMLreturn (caml_copy_int64(mark));
}

value caml_zmq_getsockopt_bytes(value socket, int option_name) {
    CAMLparam1 (socket);
    char buffer[256];
    size_t buffer_size = sizeof (buffer);
    int result = zmq_getsockopt (CAML_ZMQ_Socket_val(socket), option_name, buffer, &buffer_size);
    buffer[result] = '\0';
    caml_zmq_raise_if (result == -1);    
    CAMLreturn (caml_copy_string(buffer));
}


struct caml_zmq_get_dispacher_row {
    char *option_id;
    int option_name;
    value (*option)(value, int);
};

static struct caml_zmq_get_dispacher_row const caml_zmq_get_dispacher[] = {
    { "High_water_mark"  , ZMQ_HWM         , &caml_zmq_getsockopt_uint64 },
    { "Swap"             , ZMQ_SWAP        , &caml_zmq_getsockopt_int64  },
    { "Affinity"         , ZMQ_AFFINITY    , &caml_zmq_getsockopt_uint64 },
    { "Identity"         , ZMQ_IDENTITY    , &caml_zmq_getsockopt_bytes  },
    { "Rate"             , ZMQ_RATE        , &caml_zmq_getsockopt_int64  },
    { "Recovery_interval", ZMQ_RECOVERY_IVL, &caml_zmq_getsockopt_int64  },
    { "Multicast_loop"   , ZMQ_MCAST_LOOP  , &caml_zmq_getsockopt_int64  },
    { "Send_buffer"      , ZMQ_SNDBUF      , &caml_zmq_getsockopt_uint64 },
    { "Recieve_buffer"   , ZMQ_RCVBUF      , &caml_zmq_getsockopt_uint64 },
    { "Recieve_more"     , ZMQ_RCVMORE     , &caml_zmq_getsockopt_int64  },
    { NULL               , 0               , NULL                        }
};

CAMLprim value caml_zmq_getsockoption(value socket, value socket_option_tag) {
    CAMLparam2 (socket, socket_option_tag);
    CAMLlocal1 (option);
    int i;
    option = caml_alloc_tuple(2);
    Store_field(option, 0, socket_option_tag);
    
    for (i = 0; caml_zmq_get_dispacher[i].option_id != NULL; i++) {
        if (caml_hash_variant(caml_zmq_get_dispacher[i].option_id) == socket_option_tag) {
            Store_field(option,
                        1,
                        caml_zmq_get_dispacher[i].option(socket, caml_zmq_get_dispacher[i].option_name));
            break;
        }
    }
    if(caml_zmq_get_dispacher[i].option_id == NULL)
        caml_failwith("Invalid option");

    CAMLreturn (option);
}

CAMLprim value caml_zmq_bind(value socket, value string_address) {
    CAMLparam2 (socket, string_address);
    int result = zmq_bind(CAML_ZMQ_Socket_val(socket), String_val(string_address));
    caml_zmq_raise_if (result == -1);
    CAMLreturn(Val_unit);
}

CAMLprim value caml_zmq_connect(value socket, value string_address) {
    CAMLparam2 (socket, string_address);
    int result = zmq_connect(CAML_ZMQ_Socket_val(socket), String_val(string_address));
    caml_zmq_raise_if (result == -1);
    CAMLreturn(Val_unit);
}

static int snd_rcv_options_table[] = { 0, ZMQ_NOBLOCK, ZMQ_SNDMORE };

CAMLprim value caml_zmq_send(value socket, value string, value snd_options) {
    CAMLparam3 (socket, string, snd_options);
    void *sock = CAML_ZMQ_Socket_val(socket);
    zmq_msg_t msg;
    int option;
    int result, close_result;

    if (Int_val(snd_options) < 0 || Int_val(snd_options) > 2)
        caml_failwith("Invalid variant range");

    option = snd_rcv_options_table[Int_val(snd_options)];
    result = zmq_msg_init_size(&msg, caml_string_length(string));
    caml_zmq_raise_if (result == -1);
    /* Doesn't copy '\0' */
    memcpy ((void *) zmq_msg_data (&msg), String_val(string), caml_string_length(string));

    caml_release_runtime_system();
    result = zmq_send (sock, &msg, option);
    caml_acquire_runtime_system();

    close_result = zmq_msg_close (&msg);
    caml_zmq_raise_if (result == -1);
    caml_zmq_raise_if (close_result == -1);
    
    CAMLreturn(Val_unit);
}

CAMLprim value caml_zmq_recv(value socket, value rcv_options) {
    CAMLparam2 (socket, rcv_options);
    CAMLlocal1 (message);
    zmq_msg_t request;
    void *sock = CAML_ZMQ_Socket_val(socket);
    int option;
    int result;
    size_t size;
    if (Int_val(rcv_options) < 0 || Int_val(rcv_options) > 2)
        caml_failwith("Invalid variant range");

    option = snd_rcv_options_table[Int_val(rcv_options)];
    result = zmq_msg_init (&request);
    caml_zmq_raise_if (result == -1);

    caml_release_runtime_system();
    result = zmq_recv (sock, &request, option);
    caml_acquire_runtime_system();

    caml_zmq_raise_if (result == -1);
    size = zmq_msg_size (&request);
    message = caml_alloc_string(size + 1);
    memcpy (String_val(message), zmq_msg_data (&request), size);
    String_val(message)[size] = '\0';
    result = zmq_msg_close(&request);
    caml_zmq_raise_if (result == -1);
    CAMLreturn (message);
}

