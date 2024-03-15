OCaml bindings for ZMQ 4.x
==========================

[![Build Status](https://github.com/issuu/ocaml-zmq/actions/workflows/workflow.yml/badge.svg)](https://github.com/issuu/ocaml-zmq/actions/workflows/workflow.yml?query=branch%3Amaster)

Dependencies
------------

  * [OPAM](http://opam.ocaml.org/)
  * OCaml >= 4.03.0
  * OCaml >= 4.04.1, Async >= v0.11.0 for zmq-async
  * Ocaml >= 5.0.0 for zmq-eio
  * Lwt >= 2.6.0 for zmq-lwt
  * libzmq (c lib) >= 4.x

Install
-------

```sh
opam install zmq
```

Uninstall
---------

```sh
opam remove zmq
```

Documentation
-------------
API documentation can be found [here](https://issuu.github.io/ocaml-zmq)

Development
-----------

With OPAM you can create a local switch which will install all dependencies automatically.

```sh
opam switch create ./ 5.1.1
make
```

License
-------

See `LICENSE.md`.

Thanks
------

To the guys from the #ocaml channel, gildor, thelema, kaustuv and many others,
wagerlabs, little-arhat and the ocaml-uint authors.
