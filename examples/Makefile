
DEPS = str,unix,uint,ZMQ

.PHONY: all clean

EXAMPLES = hwclient hwserver wuclient wuserver wuproxy taskvent taskwork tasksink

all: $(EXAMPLES)

% : %.ml
	ocamlfind ocamlc -package $(DEPS) -linkpkg -o $@ $^

clean:
	rm -f *.cm*
	rm -f $(EXAMPLES)
