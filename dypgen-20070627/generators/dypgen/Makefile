include ../../Makefile.config
SOURCES = argument.ml dypgen_parser.dyp dypgen_lexer.mll insert_linenum.mll dypgen.ml
REP = -I ../../dyplib
CAMLC = ocamlc $(REP)
PGEN = ../../generators/pgen/pgen
LIBS=dyp.cma

all: dypgen dypgen.opt

SOURCES1 = $(SOURCES:.mll=.ml)
SOURCES2 = $(SOURCES1:.dyp=.ml)
OBJS = $(SOURCES2:.ml=.cmo)
OPTOBJS = $(SOURCES2:.ml=.cmx)

dypgen: parse_tree.cmi $(OBJS)
	$(CAMLC) -o dypgen $(LIBS) $(OBJS)

ifdef CAMLOPT
dypgen.opt: parse_tree.cmi $(OPTOBJS)
	$(CAMLOPT) $(REP) -o dypgen.opt $(LIBS:.cma=.cmxa) $(OPTOBJS)
else
dypgen.opt:
endif

.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .dyp

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) $(REP) -c $<

.mll.ml:
	ocamllex $<

.dyp.ml:
	$(PGEN) $<
	$(CAMLC) -c $*.mli

clean:
	rm -f *.cm[iox] *~ .*~ *.o
	rm -f dypgen dypgen.opt sortie*
	rm -f *lexer.ml
	rm -f *parser.ml *parser.mli
