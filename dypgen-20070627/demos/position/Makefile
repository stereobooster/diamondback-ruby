include ../../Makefile.config
SOURCES = parse_tree.ml position_parser.dyp position_lexer.mll position.ml
REP = -I ../../dyplib
CAMLC = ocamlc $(REP)
DYPGEN = ../../generators/dypgen/dypgen --merge-warning
LIBS=dyp.cma

all: position

SOURCES1 = $(SOURCES:.mll=.ml)
SOURCES2 = $(SOURCES1:.dyp=.ml)
OBJS = $(SOURCES2:.ml=.cmo)

position: $(OBJS)
	$(CAMLC) -o position $(LIBS) $(OBJS)

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
	rm -f position
	rm -f *lexer.ml
	rm -f *parser.ml *parser.mli
