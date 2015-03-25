ML = src/olisp

all: $(ML).ml
	ocamlbuild $(ML).native

test: 
	ocamlbuild -I src tests/tests.native 

clean:
	rm -rf _build *.native *.byte

.PHONY: all test clean
