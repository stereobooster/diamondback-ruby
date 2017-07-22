include ../../Makefile.config
SOURCES = global_data_parser.dyp global_data_lexer.mll global_data.ml
REP = -I ../../dyplib
CAMLC = ocamlc $(REP)
DYPGEN = ../../generators/dypgen/dypgen
LIBS=dyp.cma

all: global_data

SOURCES1 = $(SOURCES:.mll=.ml)
SOURCES2 = $(SOURCES1:.dyp=.ml)
OBJS = $(SOURCES2:.ml=.cmo)

global_data: $(OBJS)
	$(CAMLC) -o global_data $(LIBS) $(OBJS)

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
	rm -f global_data
	rm -f *lexer.ml
	rm -f *parser.ml *parser.mli
