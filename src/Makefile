OCAMLC=ocamlc

OBJS=syntax.cmo typing.cmo

PROGNAME=gtlc

$(PROGNAME): $(OBJS) main.cmo
	$(OCAMLC) -o $@ $(OBJS) main.cmo

%.cmo: %.ml
	$(OCAMLC) -c $<
%.cmi: %.mli
	$(OCAMLC) -c $<
# %.cmx: %.ml
# 	$(OCAMLOPT)


clean:
	rm -f *.cm[iox]
