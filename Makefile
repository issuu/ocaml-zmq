.PHONY: build
build:
	dune build @install @examples

.PHONY: examples
examples:
	dune build @examples

# requires odoc
.PHONY: doc
doc:
	dune build @doc

.PHONY: test
test:
	dune runtest --force

.PHONY: repl
repl:
	dune utop zmq/src

.PHONY: repl-lwt
repl-lwt:
	dune utop zmq-lwt/src

.PHONY: repl-async
repl-async:
	dune utop zmq-async/src

.PHONY: all
all:
	dune build @install @examples

.PHONY: install
install:
	dune install

.PHONY: uninstall
uninstall:
	dune uninstall

.PHONY: clean
clean:
	dune clean

# requires dune-release
.PHONY: tag
tag:
	dune-release tag

.PHONY: distrib
distrib:
	dune-release distrib

.PHONY: publish
publish:
	dune-release publish

.PHONY: opam-pkg
opam-pkg:
	dune-release opam pkg
