OCaml bindings for ZMQ 4.0
==========================

[![Build Status](https://travis-ci.org/issuu/ocaml-zmq.svg?branch=master)](https://travis-ci.org/issuu/ocaml-zmq)

Dependencies
------------

  * [OPAM](http://opam.ocaml.org/)
  * OCaml >= 4.03
  * Async >= v0.9.0 for zmq-async

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

Development
-----------

With OPAM 2.x you can create a local switch which will install all dependencies automatically.

```sh
opam switch create ./ 4.06
make
```

License
-------

See `LICENSE`.

Thanks
------

To the guys from the #ocaml channel, gildor, thelema, kaustuv and many others,
wagerlabs, little-arhat and the ocaml-uint authors.
