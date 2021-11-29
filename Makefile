all:
	(cd src && $(MAKE) all)
	(cd c_src && $(MAKE) all)
	(cd c_src && $(MAKE) -f Makefile.nif prepare all)

clean:
	(cd src && $(MAKE) clean)
	(cd c_src && $(MAKE) clean)
	(cd c_src && $(MAKE) -f Makefile.nif clean)
