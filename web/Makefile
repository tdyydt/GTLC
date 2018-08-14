all: gtlc.js

## web
web.byte: ../src/lib.cma web.ml
	ocamlfind ocamlc -I ../src -package js_of_ocaml -package js_of_ocaml.ppx -linkpkg -o $@ $^

gtlc.js: web.byte
	js_of_ocaml -o $@ web.byte

clean:
	rm -f *.cm[iox]
	rm -f gtlc.js web.byte
