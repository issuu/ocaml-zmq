INSTALL_FILES = \
   META \
   src/ZMQ.mli \
   $(addprefix _obuild/zmq-lib/, ZMQ.cmi zmq-lib.a zmq-lib.cma zmq-lib.cmxa libmlzmq-lib.a)

build:
	ocp-build

uninstall:
	ocamlfind remove ZMQ

install: build
	ocamlfind install ZMQ $(INSTALL_FILES)

reinstall: uninstall install

test: build
	_obuild/unittest/unittest.asm

clean:
	ocp-build clean
