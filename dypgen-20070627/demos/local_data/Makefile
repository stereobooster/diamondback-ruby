include ../../Makefile.config
SOURCES = parse_tree.ml local_data_parser.dyp local_data_lexer.mll local_data.ml
REP = -I ../../dyplib
CAMLC = ocamlc $(REP)
DYPGEN = ../../generators/dypgen/dypgen
LIBS=dyp.cma

all: local_data

SOURCES1 = $(SOURCES:.mll=.ml)
SOURCES2 = $(SOURCES1:.dyp=.ml)
OBJS = $(SOURCES2:.ml=.cmo)

local_data: $(OBJS)
	$(CAMLC) -o local_data $(LIBS) $(OBJS)

.SUFFIXES: .ml .cmo .mll .dyp

.ml.cmo:
	$(CAMLC) -c $<

.mll.ml:
	ocamllex $<

.dyp.ml:
	$(DYPGEN) $<
	$(CAMLC) -c $*.mli

clean:
	rm -f *.cm[iox] *~ .*~ *.o
	rm -f local_data
	rm -f *lexer.ml
	rm -f *parser.ml *parser.mli
