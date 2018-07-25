build:
	dune build @install @examples

examples:
	dune build @examples

# requires odoc
doc:
	dune build @doc

test:
	dune runtest --force

repl:
	dune utop zmq/src

repl-lwt:
	dune utop zmq-lwt/src

repl-async:
	dune utop zmq-async/src

all:
	dune build @install @examples

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

gh-pages: doc
	git clone `git config --get remote.origin.url` .gh-pages --reference .
	git -C .gh-pages checkout --orphan gh-pages
	git -C .gh-pages reset
	git -C .gh-pages clean -dxf
	cp  -r _build/default/_doc/* .gh-pages
	git -C .gh-pages add .
	git -C .gh-pages config user.email 'docs@ocaml-zmq'
	git -C .gh-pages commit -m "Update Pages"
	git -C .gh-pages push origin gh-pages -f
	rm -rf .gh-pages

.PHONY: build doc test all install uninstall clean configure examples repl repl-lwt repl-async gh-pages
