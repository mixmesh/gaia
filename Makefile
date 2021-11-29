all:
	(cd src && $(MAKE) all)
	(cd c_src && $(MAKE) -f Makefile.nif all)

clean:
	(cd src && $(MAKE) clean)
	(cd c_src && $(MAKE) -f Makefile.nif clean)
