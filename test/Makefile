# -I is mandatory?
# If not: Unbound module Syntax
test: ../src/lib.cma test_typing.ml
	ocamlfind ocamlc -I ../src -o test -package oUnit -linkpkg -g $^
