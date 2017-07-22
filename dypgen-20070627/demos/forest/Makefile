include ../../Makefile.config
SOURCES = forest_parser.dyp forest_lexer.mll forest.ml
REP = -I ../../dyplib
CAMLC = ocamlc $(REP)
DYPGEN = ../../generators/dypgen/dypgen
LIBS=dyp.cma

all: forest

SOURCES1 = $(SOURCES:.mll=.ml)
SOURCES2 = $(SOURCES1:.dyp=.ml)
OBJS = $(SOURCES2:.ml=.cmo)

forest: parse_tree.cmi $(OBJS)
	$(CAMLC) -o forest $(LIBS) $(OBJS)

.SUFFIXES: .ml .mli .cmo .cmi .mll .dyp

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.mll.ml:
	ocamllex $<

.dyp.ml:
	$(DYPGEN) $<
	$(CAMLC) -c $*.mli

clean:
	rm -f *.cm[iox] *~ .*~ *.o
	rm -f forest
	rm -f *lexer.ml
	rm -f *parser.ml *parser.mli
