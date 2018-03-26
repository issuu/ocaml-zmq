next (to be decided)
--------------------

* Change build system to use jbuilder instead of oasis. This also adds proper
  support for pkg-config installed zmq.

* Import zmq-async and zmq-lwt. The old bindings async-zmq and lwt-zmq are now
  deprecated.

* Refactor zmq-async and zmq-lwt to be supported out of a single code base. This
  regularizes the interface, and dramatically improves and stability.

* Add support for and reading & writing bigarrays. This interfaces allows the
  user to reduce needless copying of packets sent by the bindings.
