include ../../Makefile.config
SOURCES = pgen_parser_param.ml pgen_lexer.mll pgen.ml
REP = -I ../../dyplib
CAMLC = ocamlc $(REP)
LIBS=dyp.cma

all: pgen

SOURCES1 = $(SOURCES:.mll=.ml)
OBJS = $(SOURCES1:.ml=.cmo)

pgen: $(OBJS)
	$(CAMLC) -o pgen $(LIBS) $(OBJS)

.SUFFIXES: .ml .cmo .mll

.ml.cmo:
	$(CAMLC) -c $<

.mll.ml:
	ocamllex $<

clean:
	rm -f *.cm[iox] *~ .*~ *.o
	rm -f pgen
	rm -f *lexer.ml
