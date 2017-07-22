DIRS=calc calc_pattern calc_nested_rule sharp merge_times_plus forest position global_data local_data local_data_partial_action demo tinyML

include Makefile.config

dypgen:
	cd dyplib; $(MAKE)
	cd generators/pgen; $(MAKE)
	cd generators/dypgen; $(MAKE)

all:
	$(MAKE) dypgen
	$(MAKE) examples

examples:
	for i in $(DIRS); do \
		(cd demos/$$i; $(MAKE) all); \
	done

clean_examples:
	for i in $(DIRS) tinyML-ulex; do \
		(cd demos/$$i; $(MAKE) clean); \
	done

install: install_opt
	mkdir -p $(BINDIR)
	cp generators/dypgen/dypgen $(BINDIR)
	cp dyp2gram.pl $(BINDIR)
	chmod +x $(BINDIR)/dyp2gram.pl
	cd dyplib; $(MAKE) install

install2: install_opt
	mkdir -p $(BINDIR)
	cp generators/dypgen/dypgen $(BINDIR)	
	cp dyp2gram.pl $(BINDIR)
	cd dyplib; $(MAKE) install2

ifdef CAMLOPT
install_opt:
	mkdir -p $(BINDIR)
	cp generators/dypgen/dypgen.opt $(BINDIR)
else
install_opt:
endif

uninstall:
	rm -rf $(BINDIR)/dypgen
	rm -rf $(BINDIR)/dypgen.opt
	rm -rf $(BINDIR)/dyp2gram.pl
	cd dyplib; $(MAKE) uninstall

uninstall2:
	rm -rf $(BINDIR)/dypgen
	rm -rf $(BINDIR)/dypgen.opt
	rm -rf $(BINDIR)/dyp2gram.pl
	cd $(DYPGENLIBDIR); rm -rf dyp.cmi dyp.cma dyp.cmxa dyp.a

clean:
	$(MAKE) clean_examples
	cd dyplib; $(MAKE) clean
	cd generators/pgen; $(MAKE) clean
	cd generators/dypgen; $(MAKE) clean
	rm -f *~ dypgen-doc.log dypgen-doc.aux dypgen-doc.toc
