/*
 * Copyright (c) 2011 Pedro Borges and contributors
 */

#ifndef CAML_ZMQ_SOCKET_H_
#define CAML_ZMQ_SOCKET_H_

#include <caml/mlvalues.h>

#define CAML_ZMQ_Socket_val(v) (*((void **) Data_custom_val(v)))

value caml_zmq_copy_socket(void *zmq_socket);

#endif  /* CAML_ZMQ_SOCKET_H_ */
