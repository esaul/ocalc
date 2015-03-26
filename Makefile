ML = src/ocalc

LFLAGS = -libs nums

all: $(ML).ml
	ocamlbuild $(LFLAGS) $(ML).native

test: 
	ocamlbuild $(LFLAGS) -I src tests/tests.native 

clean:
	rm -rf _build *.native *.byte

.PHONY: all test clean
