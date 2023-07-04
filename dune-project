(lang dune 2.7)
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
  dune-configurator)
 (conflicts
  ocaml-zmq))

(package
 (name zmq-async)
 (authors "Rudi Grinberg")
 (synopsis "Async-aware bindings to ZMQ")
 (depends
  (ocaml (>= 4.04.1))
  (zmq (= :version))
  (async_unix (>= v0.11.0))
  (async_kernel (>= v0.11.0))
  (base (>= v0.11.0))
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

 (package
 (name zmq-eio)
 (authors "Anders Fugmann")
 (synopsis "Eio aware bindings to ZMQ")
 (depends
  (ocaml (>= 4.04.1))
  (zmq (= :version))
  (eio (>= 0.10))
  (eio_unix (>= 0.9))
  (base (>= v0.11.0))
  (ounit2 :with-test)))
