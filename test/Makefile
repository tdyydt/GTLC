PROGNAME=test
all: $(PROGNAME)

$(PROGNAME): ../src/lib.cma test_typing.ml test_pp.ml test_all.ml
	ocamlfind ocamlc -I ../src -o $@ -package oUnit -linkpkg -g $^

clean:
	rm -f $(PROGNAME)
	rm -f *.cm[iox]
