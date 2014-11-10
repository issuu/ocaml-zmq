INSTALL_FILES = \
   META \
   src/ZMQ.mli \
   $(addprefix  _obuild/ZMQ/, ZMQ.a ZMQ.cma ZMQ.cmi ZMQ.cmt ZMQ.cmti ZMQ.cmx ZMQ.cmxa libmlZMQ.a)

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
