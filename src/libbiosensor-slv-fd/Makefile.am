pkglib_LTLIBRARIES = libbiosensor-slv-fd.la
libbiosensor_slv_fd_la_CPPFLAGS = -I../libbiosensor-xml -I../libbiosensor
libbiosensor_slv_fd_la_LDFLAGS = -version-info 0:0:0
libbiosensor_slv_fd_la_LIBADD  = \
    ../libbiosensor/libbiosensor.la \
    ../libbiosensor-xml/libbiosensor-xml.la

libbiosensor_slv_fd_la_SOURCES = \
    bio/fd/SolverFactory.cxx \
    bio/fd/FiniteDifferencesSolverAnalyzer.cxx \
    bio/fd/ex2d/Solver.cxx \
    bio/fd/im2d/Solver.cxx \
    bio/fd/im2d/AreaSubSolver.cxx \
    bio/fd/im2d/BoundSubSolver.cxx \
    bio/fd/im2d/CornerSubSolver.cxx

nobase_pkginclude_HEADERS = \
    biosensor-slv-fd.hxx \
    bio/fd/SolverFactory.hxx

astyle:
	astyle @ASTYLE_OPTS@ *.hxx.in `find  bio -name  "*.cxx"` `find  bio -name  "*.hxx"`

