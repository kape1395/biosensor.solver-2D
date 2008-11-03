SUBDIRS = src

doxygen:
	doxygen

test-solver: src/bio-solver/bio-solver
	src/bio-solver/bio-solver src/libbiosensor-xml/Model-example-2D.xml 

test-solver-mem:
	valgrind --suppressions=valgrind.supp local_install/bin/bio-solver  src/libbiosensor-xml/Model-example-2D.xml

