# add --dev flag here when warnings are fixed
build:
	jbuilder build @install @examples

examples:
	jbuilder build @examples

# requires odoc
doc:
	jbuilder build @odoc

test:
	jbuilder runtest

all:
	jbuilder build @install @examples

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	jbuilder clean

.PHONY: build doc test all install uninstall clean configure examples
