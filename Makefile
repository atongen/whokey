NAME=whokey

.PHONY: default all utop test clean

default: all

all:
	dune build @install
	@test -L bin || ln -s _build/install/default/bin .

# Launch utop such that it finds our library.
utop: all
	OCAMLPATH=_build/install/default/lib:$(OCAMLPATH) utop

test: all
	./bin/test

clean:
	dune clean
