#ifndef CAML_ZMQ_POLL_H_
#define CAML_ZMQ_POLL_H_

#include <caml/mlvalues.h>

#include <zmq.h>

struct caml_zmq_poll {
    zmq_pollitem_t *poll_items;
    int num_elems;
};

#define CAML_ZMQ_Poll_val(v) ((struct caml_zmq_poll *) Data_custom_val(v))

value caml_zmq_poll_of_pollitem_array(value pollitem_array);
value caml_zmq_poll(value poll, value timeout);

value CAML_ZMQ_Val_mask(short mask);
short CAML_ZMQ_Mask_val(value mask);

#endif  /* CAML_ZMQ_POLLSET_H_ */
