SUBDIRS = src
ACLOCAL_AMFLAGS = -I acinclude

#AM_LDLFAGS=$(AM_LDLFAGS) $(BOOST_FILESYSTEM_LIB) $(BOOST_LDFLAGS) -lboost_filesystem

EXTRA_DIST = \
    Doxyfile \
    valgrind.supp \
    doc/Requirements.odt \
    doc/UML.xmi

dist_bin_SCRIPTS = bin/bio-output

doxygen:
	doxygen

test-solver: src/bio-solver/bio-solver
	src/bio-solver/bio-solver src/libbiosensor-xml/Model-example-2D.xml 

test-solver-mem:
	valgrind --suppressions=valgrind.supp local_install/bin/bio-solver  src/libbiosensor-xml/Model-example-2D.xml

astyle:
	make -C src/libbiosensor astyle
	make -C src/libbiosensor-slv-fd astyle
	make -C src/bio-solver astyle
	make -C src/bio-editor astyle
