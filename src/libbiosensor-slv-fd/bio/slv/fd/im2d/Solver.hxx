#ifndef BIO_SLV_FD_IM2D_Solver_HXX
#define BIO_SLV_FD_IM2D_Solver_HXX
#include "../../../../biosensor-slv-fd.hxx"

BIO_SLV_FD_IM2D_NS_BEGIN
class Solver;
BIO_SLV_FD_IM2D_NS_END

#include "ISubSolverFactory.hxx"
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
protected:
    class SolverStateImpl;

public:
    typedef BIO_NS::Splitted2DArea<AreaSubSolver*, BoundSubSolver*, CornerSubSolver*> SplittedSolver;

private:

    BIO_CFG_NS::BoundAnalyzer* boundAnalyzer;
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer;
    BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer* fdAnalyzer;
    BIO_SLV_NS::ITransducer* transducer;
    SplittedSolver* subSolvers;
    DataModel* dataModel;
    SolverStateImpl* solverState;

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
        BIO_NS::IFactory* factory,
        BIO_SLV_FD_IM2D_NS::ISubSolverFactory* subSolverFactory
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
     *  Get state for the solver.
     */
    virtual BIO_SLV_NS::ISolverState* getState();

    /**
     *  Returns subSolvers.
     */
    SplittedSolver* getSubSolvers();

    virtual BIO_CFG_NS::BoundAnalyzer* getBoundAnalyzer()
    {
        return boundAnalyzer;
    }

    virtual BIO_CFG_NS::StructureAnalyzer* getStructAnalyzer()
    {
        return structAnalyzer;
    }

    virtual BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer* getFDAnalyzer()
    {
        return fdAnalyzer;
    }

protected:

    /**
     *  Implementation of AbstractIterativeSolver::solveIteration().
     *  This method solves one full iteration (step in time).
     */
    virtual void solveIteration();


    class SolverStateImpl : public BIO_SLV_NS::ISolverState
    {
    private:
        Solver* solver;
    public:
        SolverStateImpl(Solver* solver)
        {
            this->solver = solver;
        }
        virtual ~SolverStateImpl()
        {
            //  Nothing.
        }
        virtual double getTime()
        {
            return solver->getSolvedTime();
        }
        virtual long getIteration()
        {
            return solver->getSolvedIterationCount();
        }
        virtual BIO_DM_NS::IDataModel* getData()
        {
            return solver->getData();
        }
    };
};



BIO_SLV_FD_IM2D_NS_END

#endif
