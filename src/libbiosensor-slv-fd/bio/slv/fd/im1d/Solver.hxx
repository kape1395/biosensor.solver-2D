#ifndef BIO_SLV_FD_IM1D_Solver_HXX
#define BIO_SLV_FD_IM1D_Solver_HXX
#include "../../../../biosensor-slv-fd.hxx"

BIO_SLV_FD_IM1D_NS_BEGIN
class Solver;
BIO_SLV_FD_IM1D_NS_END

#include "../im2d/ISubSolverFactory.hxx"
#include "../im2d/Solver.hxx"
#include "../im2d/AreaSubSolver.hxx"
#include "../FiniteDifferencesSolverAnalyzer.hxx"

BIO_SLV_FD_IM1D_NS_BEGIN


/**
 *  Solver: one-dimensional, implemented using implicit scheme.
 */
class Solver : public BIO_SLV_FD_IM2D_NS::Solver
{

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
     *  Overrides im2d::Solver::solveIteration().
     *  This method solves one full iteration (step in time).
     *  This implementation solver vertical direction only, as in this software
     *  in the case of 1d model the axis is treated as a vertical one.
     */
    virtual void solveIteration();

};



BIO_SLV_FD_IM1D_NS_END

#endif
