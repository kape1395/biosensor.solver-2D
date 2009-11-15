#ifndef BIO_SLV_FD_IM2D_Solver_HXX
#define BIO_SLV_FD_IM2D_Solver_HXX
#include "../../../../biosensor-slv-fd.hxx"

BIO_SLV_FD_IM2D_NS_BEGIN
class Solver;
BIO_SLV_FD_IM2D_NS_END

#include "AreaSubSolver.hxx"
#include "BoundSubSolver.hxx"
#include "CornerSubSolver.hxx"
#include "DataModel.hxx"
#include "../FiniteDifferencesSolverAnalyzer.hxx"
#include <bio/IFactory.hxx>
#include <bio/Splitted2DArea.hxx>
#include <bio/cfg/BoundAnalyzer.hxx>
#include <bio/cfg/StructureAnalyzer.hxx>
#include <bio/slv/AbstractIterativeSolver.hxx>

BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *  Solver: two-dimensional, implemented using implicit scheme.
 *  Method of alternating directions is used to reduce order of ...
 */
class Solver : public BIO_SLV_NS::AbstractIterativeSolver
{
public:
    typedef BIO_NS::Splitted2DArea<AreaSubSolver*, BoundSubSolver*, CornerSubSolver*> SplittedSolver;

private:

    BIO_CFG_NS::BoundAnalyzer* boundAnalyzer;
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer;
    BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer* fdAnalyzer;
    BIO_SLV_NS::ITransducer* transducer;
    SplittedSolver* subSolvers;
    DataModel* dataModel;

    /*
     *  It is used to switch this ant previous layers.
     *  All subsolvers must look at this attribute.
     *
    bool layersAreInversed = false;
     */

public:
    /**
     *  Constructor.
     */
    Solver(
        BIO_XML_NS::model::Model* config,
        BIO_NS::IFactory* factory
    );

    /**
     *  Destructor.
     */
    virtual ~Solver();

    /**
     *  Returns data model.
     */
    virtual BIO_DM_NS::IDataModel* getData();

    /**
     *  Returns transducer.
     */
    virtual ITransducer* getTransducer();

    /**
     *  Set state for the solver.
     */
    virtual void setState(BIO_SLV_NS::ISolverState* state);

    /**
     *  Returns subSolvers.
     */
    SplittedSolver* getSubSolvers();

protected:

    /**
     *  Implementation of AbstractIterativeSolver::solveIteration().
     *  This method solves one full iteration (step in time).
     */
    virtual void solveIteration();

};



BIO_SLV_FD_IM2D_NS_END

#endif
