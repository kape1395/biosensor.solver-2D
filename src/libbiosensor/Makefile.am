pkglib_LTLIBRARIES = libbiosensor.la
libbiosensor_la_CPPFLAGS = -I../libbiosensor-xml
libbiosensor_la_LDFLAGS = -version-info 0:0:0
libbiosensor_la_LIBADD  = \
    ../libbiosensor-xml/libbiosensor-xml.la

libbiosensor_la_SOURCES = \
    bio/Exception.cxx \
    bio/cfg/BoundAnalyzer.cxx \
    bio/cfg/IConfigTransformer.cxx \
    bio/cfg/IConfigValidator.cxx \
    bio/cfg/StructureAnalyzer.cxx \
    bio/dm/CompositeSegmentSplit.cxx \
    bio/dm/ConstantSegmentSplit.cxx \
    bio/dm/IComposite1D.cxx \
    bio/dm/IComposite2D.cxx \
    bio/dm/IConcentrations.cxx \
    bio/dm/ICursor1D.cxx \
    bio/dm/ICursor2D.cxx \
    bio/dm/IDataModel.cxx \
    bio/dm/IGrid.cxx \
    bio/dm/IGrid1D.cxx \
    bio/dm/IGrid2D.cxx \
    bio/dm/ISegmentSplit.cxx \
    bio/dm/NullDM.cxx \
    bio/io/DebugSL.cxx \
    bio/io/GnuplotDataSL.cxx \
    bio/slv/AbstractIterativeSolver.cxx \
    bio/slv/AbstractSolver.cxx \
    bio/slv/AmperometricElectrode.cxx \
    bio/slv/ErrorInDataSL.cxx \
    bio/slv/IIterativeSolver.cxx \
    bio/slv/ISolver.cxx \
    bio/slv/ISolverListener.cxx \
    bio/slv/ISolverFactory.cxx \
    bio/slv/ITransducer.cxx \
    bio/slv/InvokeEverySL.cxx \
    bio/slv/NullSolver.cxx \
    bio/slv/StopAtStepSL.cxx


nobase_pkginclude_HEADERS = \
    biosensor.hxx \
    bio/Exception.hxx \
    bio/cfg/BoundAnalyzer.hxx \
    bio/cfg/IConfigTransformer.hxx \
    bio/cfg/IConfigValidator.hxx \
    bio/cfg/StructureAnalyzer.hxx \
    bio/dm/CompositeSegmentSplit.hxx \
    bio/dm/ConstantSegmentSplit.hxx \
    bio/dm/IComposite1D.hxx \
    bio/dm/IComposite2D.hxx \
    bio/dm/IConcentrations.hxx \
    bio/dm/ICursor1D.hxx \
    bio/dm/ICursor2D.hxx \
    bio/dm/IDataModel.hxx \
    bio/dm/IGrid.hxx \
    bio/dm/IGrid1D.hxx \
    bio/dm/IGrid2D.hxx \
    bio/dm/ISegmentSplit.hxx \
    bio/dm/NullDM.hxx \
    bio/io/DebugSL.hxx \
    bio/io/GnuplotDataSL.hxx \
    bio/slv/AbstractIterativeSolver.hxx \
    bio/slv/AbstractSolver.hxx \
    bio/slv/AmperometricElectrode.hxx \
    bio/slv/ErrorInDataSL.hxx \
    bio/slv/IIterativeSolver.hxx \
    bio/slv/ISolver.hxx \
    bio/slv/ISolverListener.hxx \
    bio/slv/ISolverFactory.hxx \
    bio/slv/ITransducer.hxx \
    bio/slv/InvokeEverySL.hxx \
    bio/slv/NullSolver.hxx \
    bio/slv/StopAtStepSL.hxx

EXTRA_DIST = \
    bio/Splitted1DArea.hxx \
    bio/Splitted2DArea.hxx

astyle:
	astyle @ASTYLE_OPTS@ `find  . -name  "*.cxx"` `find  . -name  "*.hxx"`

