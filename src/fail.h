/*
 * Copyright (c) 2011 Pedro Borges and contributors
 */

#ifndef CAML_ZMQ_FAIL_H_
#define CAML_ZMQ_FAIL_H_

void caml_zmq_raise(int err_no, const char *err_str, const char *name);
void caml_zmq_raise_if(int condition, const char *name);

#endif /* CAML_ZMQ_FAIL_H_ */
