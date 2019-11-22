OCaml bindings for ZMQ 4.x
==========================

[![Build Status](https://travis-ci.org/issuu/ocaml-zmq.svg?branch=master)](https://travis-ci.org/issuu/ocaml-zmq)

Dependencies
------------

  * [OPAM](http://opam.ocaml.org/)
  * OCaml >= 4.03.0, OCaml >= 4.04.1 for zmq-async
  * Async >= v0.11.0 for zmq-async
  * Lwt for zmq-lwt
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

With OPAM 2.x you can create a local switch which will install all dependencies automatically.

```sh
opam switch create ./ 4.06
make
```

License
-------

See `LICENSE.md`.

Thanks
------

To the guys from the #ocaml channel, gildor, thelema, kaustuv and many others,
wagerlabs, little-arhat and the ocaml-uint authors.
