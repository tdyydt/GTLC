OCAMLC=ocamlc
OCAMLDEP=ocamldep
OCAMLLEX=ocamllex
MENHIR=menhir
MENHIROPTIONS=--explain

PROGNAME=gtlc

# The order of files matters
# 順番を依存関係の順番にする必要がある？
# => depend を使うことで，その必要がなくなっていそう
OBJS=syntax.cmo environment.cmo parser.cmo lexer.cmo typing.cmo

DEPEND += lexer.ml parser.ml


$(PROGNAME): $(DEPEND) $(OBJS) main.cmo
	$(OCAMLC) -o $@ $(OBJS) main.cmo

%.cmo: %.ml
	$(OCAMLC) -c $<
%.cmi: %.mli
	$(OCAMLC) -c $<
# %.cmx: %.ml
# 	$(OCAMLOPT)

# parser & lexer
parser.ml parser.mli: parser.mly
	@rm -f $@
	$(MENHIR) $(MENHIROPTIONS) -v $<
	@chmod -w $@

lexer.ml: lexer.mll
	@rm -f $@
	$(OCAMLLEX) $<
	@chmod -w $@

clean:
	rm -f *.cm[iox]

# Dependencies
depend:: $(DEPEND)
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

-include .depend
