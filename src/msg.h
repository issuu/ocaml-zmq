/*
 * Copyright (c) 2011 Pedro Borges and contributors
 */

#ifndef CAML_ZMQ_MSG_H_
#define CAML_ZMQ_MSG_H_

#include <caml/mlvalues.h>

#define CAML_ZMQ_Msg_val(v) (*((void **) Data_custom_val(v)))

value caml_zmq_copy_msg(void *zmq_msg);

#endif  /* CAML_ZMQ_MSG_H_ */

