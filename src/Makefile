OCAMLFIND= ocamlfind
OCAMLC= ocamlc -g -ccopt -g
OCAMLOPT= ocamlopt -g -ccopt -g
OCAMLMKLIB= ocamlmklib

LIB_NAME= ZMQ
# For case insensitive filesystems -lzmq = -lZMQ
# fooling the linker in later stages
LIB_NAME_ALT= ZMQB
META_FILE= META
DEPS= -package unix,uint.uint64
OBJS= socket.o fail.o context.o uint64.o caml_zmq_stubs.o poll.o
INSTALL_FILES=*.mli *.cmi *.cma *.cmx *.cmxa *.a *.so

all: native bytecode

native: ZMQB.cmxa

ZMQB.cmxa: ZMQ.cmi ZMQ.cmx $(OBJS)
	$(OCAMLMKLIB) -o $(LIB_NAME_ALT) ZMQ.cmx $(OBJS) -lzmq

bytecode: ZMQB.cma

ZMQB.cma: ZMQ.cmi ZMQ.cmo $(OBJS)
	$(OCAMLMKLIB) -o $(LIB_NAME_ALT) ZMQ.cmo $(OBJS) -lzmq

%.cmi: %.mli
	$(OCAMLFIND) $(OCAMLC) $(DEPS) -c $<

%.cmo: %.ml
	$(OCAMLFIND) $(OCAMLC) $(DEPS) -c $<

%.cmx: %.ml
	$(OCAMLFIND) $(OCAMLOPT) $(DEPS) -c $<

%.o: %.c
	$(OCAMLFIND) $(OCAMLC) -c $<

clean:
	rm -rf *.o *.cmi *.cmo *.cmx *.a *.so *.cma *.cmxa


install: all
	$(OCAMLFIND) install $(LIB_NAME) $(META_FILE) $(INSTALL_FILES)

uninstall:
	$(OCAMLFIND) remove $(LIB_NAME)

reinstall: uninstall install

.PHONY: clean
