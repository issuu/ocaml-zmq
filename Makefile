OCAMLFIND_FILES = ZMQ.a ZMQ.cma ZMQ.cmi ZMQ.cmt ZMQ.cmti ZMQ.cmx ZMQ.cmxa libmlZMQ.a

build:
	ocp-build

uninstall:
	ocamlfind remove ZMQ

install: build
	ocamlfind install ZMQ META src/ZMQ.mli $(addprefix _obuild/ZMQ/, $(OCAMLFIND_FILES))

reinstall: uninstall install
