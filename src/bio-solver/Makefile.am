bin_PROGRAMS = bio-solver

bio_solver_CPPFLAGS = \
    -I../libbiosensor \
    -I../libbiosensor-xml \
    -I../libbiosensor-slv-fd

bio_solver_LDADD = \
    ../libbiosensor/libbiosensor.la \
    ../libbiosensor-xml/libbiosensor-xml.la \
    ../libbiosensor-slv-fd/libbiosensor-slv-fd.la

bio_solver_SOURCES = \
    main.cxx

dist_man1_MANS = bio-solver.man

astyle:
	astyle @ASTYLE_OPTS@ `find  . -name  "*.cxx"` `find  . -name  "*.hxx"`

