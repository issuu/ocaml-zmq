(** @deprecated This module is deprecated and will be removed in future
    versions.  Please use the `Zmq` module instead. *)

[@@@ocaml.deprecated
  "ZMQ is deprecated and will be removed in a future release.  Please use the \
   Zmq module instead."]

include module type of struct
    include Zmq
end
