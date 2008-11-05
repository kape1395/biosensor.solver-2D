pkglib_LTLIBRARIES = libbiosensor.la
libbiosensor_la_CPPFLAGS = -I../libbiosensor-xml
libbiosensor_la_LDFLAGS = -version-info 0:0:0
libbiosensor_la_LIBADD  = \
    ../libbiosensor-xml/libbiosensor-xml.la

libbiosensor_la_SOURCES = \
    bio/Exception.cxx \
    bio/cfg/IConfigAnalyzer.cxx \
    bio/cfg/IConfigTransformer.cxx \
    bio/cfg/IConfigValidator.cxx \
    bio/cfg/StructureAnalyzer.cxx \
    bio/dm/IComposite1D.cxx \
    bio/dm/IComposite1DArea.cxx \
    bio/dm/IComposite1DBound.cxx \
    bio/dm/IComposite2D.cxx \
    bio/dm/IComposite2DArea.cxx \
    bio/dm/IComposite2DBound.cxx \
    bio/dm/IComposite2DCorner.cxx \
    bio/dm/IConcentrations.cxx \
    bio/dm/ICursor1D.cxx \
    bio/dm/ICursor2D.cxx \
    bio/dm/IDataModel.cxx \
    bio/dm/IGrid.cxx \
    bio/dm/IGrid1D.cxx \
    bio/dm/IGrid2D.cxx \
    bio/dm/NullDM.cxx \
    bio/slv/AbstractIterativeSolver.cxx \
    bio/slv/AbstractSolver.cxx \
    bio/slv/IIterativeSolver.cxx \
    bio/slv/ISolver.cxx \
    bio/slv/ISolverListener.cxx \
    bio/slv/ISolverFactory.cxx \
    bio/slv/NullSolver.cxx

nobase_pkginclude_HEADERS = \
    biosensor.hxx \
    bio/Exception.hxx \
    bio/cfg/IConfigAnalyzer.hxx \
    bio/cfg/IConfigTransformer.hxx \
    bio/cfg/IConfigValidator.hxx \
    bio/cfg/StructureAnalyzer.hxx \
    bio/dm/IComposite1D.hxx \
    bio/dm/IComposite1DArea.hxx \
    bio/dm/IComposite1DBound.hxx \
    bio/dm/IComposite2D.hxx \
    bio/dm/IComposite2DArea.hxx \
    bio/dm/IComposite2DBound.hxx \
    bio/dm/IComposite2DCorner.hxx \
    bio/dm/IConcentrations.hxx \
    bio/dm/ICursor1D.hxx \
    bio/dm/ICursor2D.hxx \
    bio/dm/IDataModel.hxx \
    bio/dm/IGrid.hxx \
    bio/dm/IGrid1D.hxx \
    bio/dm/IGrid2D.hxx \
    bio/dm/NullDM.hxx \
    bio/slv/AbstractIterativeSolver.hxx \
    bio/slv/AbstractSolver.hxx \
    bio/slv/IIterativeSolver.hxx \
    bio/slv/ISolver.hxx \
    bio/slv/ISolverListener.hxx \
    bio/slv/ISolverFactory.hxx \
    bio/slv/NullSolver.hxx

astyle:
	astyle @ASTYLE_OPTS@ `find  . -name  "*.cxx"` `find  . -name  "*.hxx"`

