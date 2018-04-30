# add --dev flag here when warnings are fixed
build:
	jbuilder build @install @examples --dev

examples:
	jbuilder build @examples

# requires odoc
doc:
	jbuilder build @doc

test:
	jbuilder runtest --dev --force

repl:
	jbuilder utop zmq/src

repl-lwt:
	jbuilder utop zmq-lwt/src

repl-async:
	jbuilder utop zmq-async/src

all:
	jbuilder build @install @examples

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	jbuilder clean

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
