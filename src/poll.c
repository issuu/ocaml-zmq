#include "poll.h"

#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/threads.h>
#include <caml/fail.h>
#include <caml/alloc.h>


#include "fail.h"
#include "socket.h"

static void custom_finalize_poll(value poll) {
    free(CAML_ZMQ_Poll_val(poll)->poll_items);
}

static struct custom_operations caml_zmq_poll_ops = {
    "org.zeromq.poll",
    custom_finalize_poll,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

CAMLprim value caml_zmq_poll_of_pollitem_array(value pollitem_array) {
    CAMLparam1 (pollitem_array);
    CAMLlocal2 (poll, current_elem);

    int n = Wosize_val(pollitem_array);
    zmq_pollitem_t *items = malloc(sizeof(zmq_pollitem_t) * n);
    caml_zmq_raise_if(items == NULL);
    int i;
    for(i = 0; i < n; i++) {
        current_elem = Field(pollitem_array, n);
        items[i].socket = CAML_ZMQ_Socket_val(Field(current_elem, 0));
        items[i].events = CAML_ZMQ_Mask_val(Field(current_elem, 1));
    }

    poll= caml_alloc_custom(&caml_zmq_poll_ops, 
                            sizeof(struct caml_zmq_poll), 
                            0, 1);
    CAML_ZMQ_Poll_val(poll)->num_elems = n;
    CAML_ZMQ_Poll_val(poll)->poll_items = items;
    CAMLreturn (poll);
}

/* Sync with variant declaration */
enum caml_zmq_event_mask {
    In = 0,
    Out,
    In_out
};

value CAML_ZMQ_Val_mask(short mask) {
    if(mask & ZMQ_POLLIN) {
        if(mask & ZMQ_POLLOUT) {
            return Val_int(In_out);
        }
        return Val_int(In);
    }
    if(mask & ZMQ_POLLOUT) {
        return Val_int(Out);
    }
    caml_failwith("Invalid event mask detected. Should be impossible to reach.");
}

short CAML_ZMQ_Mask_val (value mask) {
    switch(Int_val(mask)) {
        case In: return ZMQ_POLLIN;
        case Out: return ZMQ_POLLOUT;
        case In_out: return ZMQ_POLLOUT | ZMQ_POLLIN;
    }
    caml_failwith("Invalid event mask detected. Should be impossible to reach.");
}

CAMLprim value caml_zmq_poll(value poll, value timeout) {
    CAMLparam2 (poll, timeout);
    CAMLlocal2 (poll_itemarray, curr_elem);
    int n = CAML_ZMQ_Poll_val(poll)->num_elems;
    zmq_pollitem_t *items = CAML_ZMQ_Poll_val(poll)->poll_items;
    int tm = Int_val(timeout);

    caml_release_runtime_system();
    int num_event_sockets = zmq_poll(items, n, tm);
    caml_acquire_runtime_system();

    caml_zmq_raise_if(num_event_sockets == -1);
    if(num_event_sockets == 0) { /* It's invalid to allocate a zero sized array */
        poll_itemarray = Atom(0);
    } else {
        poll_itemarray = caml_alloc_tuple(num_event_sockets);
        int i, j;
        for(i = 0, j = 0; i < num_event_sockets; i++) {
            while(!((items[j].revents | ZMQ_POLLIN) || (items[j].revents | ZMQ_POLLOUT))) {
                j++;
            }
            curr_elem = caml_alloc_tuple(2);
            Store_field(curr_elem, 0, caml_zmq_copy_socket(items[j].socket));
            Store_field(curr_elem, 1, CAML_ZMQ_Val_mask(items[j].revents));
            Store_field(poll_itemarray, i, curr_elem);
            j++;
        }
    }

    CAMLreturn (poll_itemarray);
}
