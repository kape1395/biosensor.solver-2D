SUBDIRS = src

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
