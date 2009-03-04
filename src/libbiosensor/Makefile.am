pkglib_LTLIBRARIES = libbiosensor.la
libbiosensor_la_CPPFLAGS = -I../libbiosensor-xml
libbiosensor_la_LDFLAGS = -version-info 0:0:0
libbiosensor_la_LIBADD  = \
    ../libbiosensor-xml/libbiosensor-xml.la

libbiosensor_la_SOURCES = \
    bio/DelegatingFactory.cxx \
    bio/Exception.cxx \
    bio/IFactory.cxx \
    bio/MainFactory.cxx \
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
    bio/io/AveragedConcentration.cxx \
    bio/io/ConcentrationProfile.cxx \
    bio/io/CurrentDensity.cxx \
    bio/io/FilesystemContext.cxx \
    bio/io/IOutput.cxx \
    bio/io/IContext.cxx \
    bio/io/IRepeatable.cxx \
    bio/io/SteadyStateHalfTime.cxx \
    bio/slv/AbstractIterativeSolver.cxx \
    bio/slv/AbstractSolver.cxx \
    bio/slv/IIterativeSolver.cxx \
    bio/slv/ISolver.cxx \
    bio/slv/ISolverListener.cxx \
    bio/slv/ITransducer.cxx \
    bio/slv/InvokeEveryTimeStep.cxx \
    bio/slv/InvokeNotBefore.cxx \
    bio/slv/NullSolver.cxx \
    bio/slv/StopAtSpecifiedPoint.cxx \
    bio/slv/StopByCurrentDensityGradient.cxx \
    bio/slv/StopIfSumOfConcentrationsNonConst.cxx \
    bio/slv/StopIfInvalidConcentrations.cxx \
    bio/trd/AmperometricElectrode2DOnBound.cxx \
    bio/trd/AmperometricInjectedElectrode2D.cxx \
    bio/trd/ConcentrationIntegralOverArea.cxx


nobase_pkginclude_HEADERS = \
    biosensor.hxx \
    bio/DelegatingFactory.hxx \
    bio/Exception.hxx \
    bio/IFactory.hxx \
    bio/MainFactory.hxx \
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
    bio/io/AveragedConcentration.hxx \
    bio/io/ConcentrationProfile.hxx \
    bio/io/CurrentDensity.hxx \
    bio/io/FilesystemContext.hxx \
    bio/io/IOutput.hxx \
    bio/io/IContext.hxx \
    bio/io/IRepeatable.hxx \
    bio/io/SteadyStateHalfTime.hxx \
    bio/slv/AbstractIterativeSolver.hxx \
    bio/slv/AbstractSolver.hxx \
    bio/slv/IIterativeSolver.hxx \
    bio/slv/ISolver.hxx \
    bio/slv/ISolverListener.hxx \
    bio/slv/ITransducer.hxx \
    bio/slv/InvokeEveryTimeStep.hxx \
    bio/slv/InvokeNotBefore.hxx \
    bio/slv/NullSolver.hxx \
    bio/slv/StopAtSpecifiedPoint.hxx \
    bio/slv/StopByCurrentDensityGradient.hxx \
    bio/slv/StopIfInvalidConcentrations.hxx \
    bio/slv/StopIfSumOfConcentrationsNonConst.hxx \
    bio/trd/AmperometricElectrode2DOnBound.hxx \
    bio/trd/AmperometricInjectedElectrode2D.hxx \
    bio/trd/ConcentrationIntegralOverArea.hxx

EXTRA_DIST = \
    bio/Logging.hxx \
    bio/Splitted1DArea.hxx \
    bio/Splitted2DArea.hxx

astyle:
	astyle @ASTYLE_OPTS@ `find  . -name  "*.cxx"` `find  . -name  "*.hxx"`

