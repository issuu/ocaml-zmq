/*
 * Copyright (c) 2011 Pedro Borges and contributors
 */

#ifndef CAML_ZMQ_CONTEXT_H_
#define CAML_ZMQ_CONTEXT_H_

#include <caml/mlvalues.h>

#define CAML_ZMQ_Context_val(v) (*((void **) Data_custom_val(v)))

value caml_zmq_copy_context(void *zmq_context);

#endif  /* CAML_ZMQ_CONTEXT_H_ */
