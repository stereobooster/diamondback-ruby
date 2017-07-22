include ../../Makefile.config
SOURCES = argument.ml parse_tree.ml tinyML_parser.dyp tinyML_lexer.mll tinyML.ml
REP = -I ../../dyplib
CAMLC = ocamlc -rectypes $(REP)
DYPGEN = ../../generators/dypgen/dypgen --pv-obj --pv-token
LIBS=dyp.cma

all: tinyML

opt : tinyML.opt

SOURCES1 = $(SOURCES:.mll=.ml)
SOURCES2 = $(SOURCES1:.dyp=.ml)
OBJS = $(SOURCES2:.ml=.cmo)
OPTOBJS = $(SOURCES2:.ml=.cmx)

tinyML: $(OBJS) grammar.txt
	$(CAMLC) -o tinyML $(LIBS) $(OBJS)

ifdef CAMLOPT
tinyML.opt: $(OPTOBJS)
	$(CAMLOPT) $(REP) -o tinyML.opt $(LIBS:.cma=.cmxa) $(OPTOBJS)
else
tinyML.opt:
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
	$(DYPGEN) $<
	$(CAMLC) -c $*.mli

grammar.txt: tinyML_parser.dyp
	chmod +x ../../dyp2gram.pl
	../../dyp2gram.pl tinyML_parser.dyp grammar.txt

clean:
	rm -f *.cm[iox] *~ .*~ *.o
	rm -f tinyML
	rm -f *lexer.ml
	rm -f *parser.ml *parser.mli
	rm -f grammar.txt
