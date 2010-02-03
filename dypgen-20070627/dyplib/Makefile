include ../Makefile.config

REP =
CAMLC=ocamlc
OCAMLFIND = ocamlfind

all: dyp dyp.opt


dyp: dyp.cmi gs.cmo priority_by_relation.cmo automaton.cmo dyp.cmo
	$(CAMLC) -a -o dyp.cma gs.cmo priority_by_relation.cmo automaton.cmo dyp.cmo


ifdef CAMLOPT
dyp.opt: dyp.cmi gs.cmx priority_by_relation.cmx automaton.cmx dyp.cmx
	$(CAMLOPT) -a -o dyp.cmxa gs.cmx priority_by_relation.cmx automaton.cmx dyp.cmx
else
dyp.opt:
endif

install: uninstall
	$(OCAMLFIND) install dyp META dyp.cma dyp.cmxa dyp.a dyp.cmi

uninstall:
	$(OCAMLFIND) remove dyp

install2: install_dyp install_dyp.opt
	cp dyp.cmi $(DYPGENLIBDIR)

install_dyp:
	mkdir -p $(DYPGENLIBDIR)
	cp dyp.cma $(DYPGENLIBDIR)

ifdef CAMLOPT
install_dyp.opt:
	mkdir -p $(DYPGENLIBDIR)
	cp dyp.cmxa $(DYPGENLIBDIR)
	cp dyp.a $(DYPGENLIBDIR)
else
install_dyp.opt:
endif


ifdef CAMLOPT
.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll
else
.SUFFIXES: .ml .mli .cmo .cmi .mll
endif

.ml.cmo:
	$(CAMLC) $(REP) -c $<

.mli.cmi:
	$(CAMLC) -c $<

ifdef CAMLOPT
.ml.cmx:
	$(CAMLOPT) $(REP) -c $<
endif

clean:
	rm -f *.cm[iox] *~ .*~ *.o *.a *.cma *.cmxa
