pkglib_LTLIBRARIES = libbiosensor-slv-fd.la
libbiosensor_slv_fd_la_CPPFLAGS = -I../libbiosensor-xml -I../libbiosensor
libbiosensor_slv_fd_la_LDFLAGS = -version-info 0:0:0
libbiosensor_slv_fd_la_LIBADD  = \
    ../libbiosensor/libbiosensor.la \
    ../libbiosensor-xml/libbiosensor-xml.la

libbiosensor_slv_fd_la_SOURCES = \
    bio/slv/fd/Factory.cxx \
    bio/slv/fd/FiniteDifferencesSolverAnalyzer.cxx \
    bio/slv/fd/ex2d/Solver.cxx \
    bio/slv/fd/im2d/Solver.cxx \
    bio/slv/fd/im2d/DataModel.cxx \
    bio/slv/fd/im2d/AreaSubSolver.cxx \
    bio/slv/fd/im2d/BoundSubSolver.cxx \
    bio/slv/fd/im2d/CornerSubSolver.cxx \
    bio/slv/fd/im2d/ConstantCondition.cxx \
    bio/slv/fd/im2d/MergeCondition.cxx \
    bio/slv/fd/im2d/WallCondition.cxx


nobase_pkginclude_HEADERS = \
    biosensor-slv-fd.hxx \
    bio/slv/fd/Factory.hxx

EXTRA_DIST = \
    bio/slv/fd/Factory.hxx \
    bio/slv/fd/FiniteDifferencesSolverAnalyzer.hxx \
    bio/slv/fd/ex2d/Solver.hxx \
    bio/slv/fd/im2d/Solver.hxx \
    bio/slv/fd/im2d/DataModel.hxx \
    bio/slv/fd/im2d/AreaSubSolver.hxx \
    bio/slv/fd/im2d/BoundSubSolver.hxx \
    bio/slv/fd/im2d/CornerSubSolver.hxx \
    bio/slv/fd/im2d/IAreaEgdeData.hxx \
    bio/slv/fd/im2d/IBoundCondition.hxx \
    bio/slv/fd/im2d/ConstantCondition.hxx \
    bio/slv/fd/im2d/MergeCondition.hxx \
    bio/slv/fd/im2d/WallCondition.hxx


astyle:
	astyle @ASTYLE_OPTS@ *.hxx.in `find  bio -name  "*.cxx"` `find  bio -name  "*.hxx"`

