OCAMLC=ocamlc
OCAMLDEP=ocamldep
OCAMLLEX=ocamllex
MENHIR=menhir
MENHIROPTIONS=--explain --infer

PROGNAME=gtlc

# The order of files matters ??
OBJS=util.cmo syntax.cmo environment.cmo parser.cmo lexer.cmo stringify.cmo \
     typing.cmo translate.cmo eval.cmo

DEPEND += lexer.ml parser.ml

all: $(PROGNAME)

$(PROGNAME): lib.cma main.cmo
	$(OCAMLC) -o $@ $^

# library (.cma)
lib.cma: $(DEPEND) $(OBJS)
	$(OCAMLC) -a -o $@ $(OBJS)


# Common rules (replacement of suffix rules)
%.cmo: %.ml
	$(OCAMLC) -c $<
%.cmi: %.mli
	$(OCAMLC) -c $<
# %.cmx: %.ml
# 	$(OCAMLOPT)


# parser & lexer
# NOTE: You have to add dependencies of parser.mly by hand,
# and their order matters.
parser.ml parser.mli: parser.mly util.cmo syntax.cmo
	@rm -f $@
	$(MENHIR) $(MENHIROPTIONS) -v $<
	@chmod -w $@

lexer.ml: lexer.mll
	@rm -f $@
	$(OCAMLLEX) $<
	@chmod -w $@

clean:
	rm -f $(PROGNAME)
	rm -f *.cm[iox]
	rm -f parser.ml parser.mli parser.output lexer.ml
	rm -f .depend

# Dependencies
depend:: $(DEPEND)
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

-include .depend
