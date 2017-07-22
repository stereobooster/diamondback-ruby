include ../../Makefile.config
SOURCES = argument.ml parse_tree.ml tinyML_parser.dyp tinyML_lexer.ml tinyML.ml
REP = -I ../../dyplib
CAMLC = ocamlfind ocamlc -package ulex $(REP) -rectypes
DYPGEN = ../../generators/dypgen/dypgen --lexer ulex
LIBS=dyp.cma ulexing.cma

all: tinyML

SOURCES1 = $(SOURCES:.dyp=.ml)
OBJS = $(SOURCES1:.ml=.cmo)

tinyML: $(OBJS)
	$(CAMLC) -o tinyML $(LIBS) $(OBJS)

.SUFFIXES: .ml .mli .cmo .cmi .dyp

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.dyp.ml:
	$(DYPGEN) $<
	$(CAMLC) -c $*.mli

clean:
	rm -f *.cm[iox] *~ .*~ *.o
	rm -f tinyML
	rm -f *parser.ml *parser.mli

tinyML_lexer.cmo:tinyML_parser.cmi
	$(CAMLC) -c -syntax camlp4o tinyML_lexer.ml
