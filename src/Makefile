OCAMLC=ocamlc
OCAMLDEP=ocamldep
OCAMLLEX=ocamllex
MENHIR=menhir
MENHIROPTIONS=--explain

PROGNAME=gtlc

# Without depend, the order of files matters
OBJS=util.cmo syntax.cmo environment.cmo parser.cmo lexer.cmo typing.cmo \
     translate.cmo eval.cmo

DEPEND += lexer.ml parser.ml

all: $(PROGNAME)

# $(PROGNAME): $(DEPEND) $(OBJS) main.cmo
# 	$(OCAMLC) -o $@ $(OBJS) main.cmo

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
