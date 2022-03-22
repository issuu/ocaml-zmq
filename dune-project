(lang dune 2.0)
(name zmq)

(generate_opam_files true)

(source (github issuu/ocaml-zmq))
(license MIT)
(maintainers "Anders Fugmann <anders@fugmann.net>")

(package
 (name zmq)
 (authors "Anders Fugmann" "Pedro Borges" "Peter Zotov")
 (synopsis "OCaml bindings for ZeroMQ 4.x")
 (description "This library contains basic bindings for ZMQ.
Lwt aware bindings to zmq are availble though package zmq-lwt
Async aware bindings to zmq are available though package zmq-async")
 (depends
  (ocaml (>= 4.03.0))
  conf-zmq
  (ounit2 :with-test)
  dune-configurator
  (stdint (>= 0.4.2)))
 (conflicts
  ocaml-zmq))

(package
 (name zmq-async)
 (authors "Rudi Grinberg")
 (synopsis "Async-aware bindings to ZMQ")
 (depends
  (ocaml (>= 4.04.1))
  (zmq (= :version))
  (async_unix (and (>= v0.11.0) (< v0.15)))
  (async_kernel (and (>= v0.11.0) (< v0.15)))
  (base (and (>= v0.11.0) (< v0.15)))
  (ounit2 :with-test)))

(package
 (name zmq-lwt)
 (authors "Anders Fugmann <anders@fugmann.net>")
 (synopsis "Lwt-aware bindings to ZMQ")
 (depends
  (ocaml (>= 4.03.0))
  (zmq (= :version))
  (lwt (>= 2.6.0))
  (ounit2 :with-test)))
