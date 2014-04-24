/*
 * Copyright (c) 2011 Pedro Borges and contributors
 */

#ifndef CAML_ZMQ_FAIL_H_
#define CAML_ZMQ_FAIL_H_

void caml_zmq_raise(int err_no, const char *err_path);
void caml_zmq_raise_if(int condition, char *err_path);

#endif  /* CAML_ZMQ_FAIL_H_ */
