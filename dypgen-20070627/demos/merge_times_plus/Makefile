include ../../Makefile.config
SOURCES = parse_tree.ml merge_tp_parser.dyp merge_tp_lexer.mll merge_tp.ml
REP = -I ../../dyplib
CAMLC = ocamlc $(REP)
DYPGEN = ../../generators/dypgen/dypgen
LIBS=dyp.cma

all: merge_tp

SOURCES1 = $(SOURCES:.mll=.ml)
SOURCES2 = $(SOURCES1:.dyp=.ml)
OBJS = $(SOURCES2:.ml=.cmo)

merge_tp: $(OBJS)
	$(CAMLC) -o merge_tp $(LIBS) $(OBJS)

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
	rm -f merge_tp
	rm -f *lexer.ml
	rm -f *parser.ml *parser.mli
