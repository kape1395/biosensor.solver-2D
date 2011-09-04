pkglib_LTLIBRARIES = libbiosensor.la
libbiosensor_la_CPPFLAGS = -I../libbiosensor-xml
libbiosensor_la_LDFLAGS = -version-info 0:0:0
libbiosensor_la_LIBADD  = \
    ../libbiosensor-xml/libbiosensor-xml.la

libbiosensor_la_SOURCES = \
    bio/DelegatingFactory.cxx \
    bio/Exception.cxx \
    bio/MainFactory.cxx \
    bio/cfg/BoundAnalyzer.cxx \
    bio/cfg/ReactionAnalyzer.cxx \
    bio/cfg/StructureAnalyzer.cxx \
    bio/dm/CompositeSegmentSplit.cxx \
    bio/dm/ConstantSegmentSplit.cxx \
    bio/dm/Cursor2DOpenBounds.cxx \
    bio/dm/Cursor2DWithoutBounds.cxx \
    bio/dm/NullDM.cxx \
    bio/io/AveragedConcentration.cxx \
    bio/io/ConcentrationProfile.cxx \
    bio/io/ConcentrationProfileInMemory.cxx \
    bio/io/ConcentrationProfileReader.cxx \
    bio/io/CurrentDensity.cxx \
    bio/io/FilesystemContext.cxx \
    bio/slv/AbstractIterativeSolver.cxx \
    bio/slv/AbstractSolver.cxx \
    bio/slv/AdjustTimeStepAdaptively.cxx \
    bio/slv/AdjustTimeStepByFactor.cxx \
    bio/slv/InvokeEveryTimeStep.cxx \
    bio/slv/InvokeNotBefore.cxx \
    bio/slv/NullSolver.cxx \
    bio/slv/StopAtSpecifiedPoint.cxx \
    bio/slv/StopByCurrentDensityGradient.cxx \
    bio/slv/StopIfConcentrationsOscillateBySpace.cxx \
    bio/slv/StopIfInvalidConcentrations.cxx \
    bio/slv/StopIfSumOfConcentrationsNonConst.cxx \
    bio/trd/AmperometricElectrode2DOnBound.cxx \
    bio/trd/AmperometricInjectedElectrode2D.cxx \
    bio/trd/CompositeElectrode.cxx \
    bio/trd/IntegralOverArea.cxx \
    bio/trd/IntegratedConcentration.cxx \
    bio/trd/IntegratedReaction.cxx \
    bio/trd/IntegratedReactionRedOx.cxx


nobase_pkginclude_HEADERS = \
    biosensor.hxx \
    bio/DelegatingFactory.hxx \
    bio/Exception.hxx \
    bio/IFactory.hxx \
    bio/MainFactory.hxx \
    bio/cfg/BoundAnalyzer.hxx \
    bio/cfg/IConfigTransformer.hxx \
    bio/cfg/IConfigValidator.hxx \
    bio/cfg/ReactionAnalyzer.hxx \
    bio/cfg/StructureAnalyzer.hxx \
    bio/dm/AbstractCursor2D.hxx \
    bio/dm/CompositeSegmentSplit.hxx \
    bio/dm/ConstantSegmentSplit.hxx \
    bio/dm/Cursor2DOpenBounds.hxx \
    bio/dm/Cursor2DWithoutBounds.hxx \
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
    bio/io/ConcentrationProfileInMemory.hxx \
    bio/io/ConcentrationProfileReader.hxx \
    bio/io/CurrentDensity.hxx \
    bio/io/FilesystemContext.hxx \
    bio/io/IOutput.hxx \
    bio/io/IContext.hxx \
    bio/io/IRepeatable.hxx \
    bio/slv/AbstractIterativeSolver.hxx \
    bio/slv/AbstractSolver.hxx \
    bio/slv/AdjustTimeStepAdaptively.hxx \
    bio/slv/AdjustTimeStepByFactor.hxx \
    bio/slv/IIterativeSolver.hxx \
    bio/slv/ISolver.hxx \
    bio/slv/ISolverListener.hxx \
    bio/slv/ISolverState.hxx \
    bio/slv/ISolverStateHolder.hxx \
    bio/slv/ITransducer.hxx \
    bio/slv/InvokeEveryTimeStep.hxx \
    bio/slv/InvokeNotBefore.hxx \
    bio/slv/NullSolver.hxx \
    bio/slv/StopAtSpecifiedPoint.hxx \
    bio/slv/StopByCurrentDensityGradient.hxx \
    bio/slv/StopIfConcentrationsOscillateBySpace.hxx \
    bio/slv/StopIfInvalidConcentrations.hxx \
    bio/slv/StopIfSumOfConcentrationsNonConst.hxx \
    bio/trd/AmperometricElectrode2DOnBound.hxx \
    bio/trd/AmperometricInjectedElectrode2D.hxx \
    bio/trd/CompositeElectrode.hxx \
    bio/trd/IntegralOverArea.hxx \
    bio/trd/IIntegratedExpression.hxx \
    bio/trd/IntegratedConcentration.hxx \
    bio/trd/IntegratedReaction.hxx \
    bio/trd/IntegratedReactionRedOx.hxx

EXTRA_DIST = \
    bio/Logging.hxx \
    bio/Splitted1DArea.hxx \
    bio/Splitted2DArea.hxx

astyle:
	astyle @ASTYLE_OPTS@ `find  . -name  "*.cxx"` `find  . -name  "*.hxx"`

