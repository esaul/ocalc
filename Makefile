LIBS = str

LFLAGS = -libs $(LIBS)

ML = src/olisp

ocamurl: $(ML).ml
	ocamlbuild $(LFLAGS) $(CFLAGS) $(ML).native

clean:
	rm -rf _build *.native *.byte
