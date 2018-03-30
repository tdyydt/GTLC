OCAMLC=ocamlc

# The order of files matters
# 順番を依存関係の順番にする必要がある？
OBJS=environment.cmo syntax.cmo typing.cmo

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
