#ifndef BIO_SLV_FD_IM1D_AreaSubSolver_HXX
#define BIO_SLV_FD_IM1D_AreaSubSolver_HXX
#include "../../../../biosensor-slv-fd.hxx"

BIO_SLV_FD_IM1D_NS_BEGIN
class AreaSubSolver;
BIO_SLV_FD_IM1D_NS_END

#include "Solver.hxx"
#include "../im2d/Solver.hxx"
#include "../im2d/AreaSubSolver.hxx"

BIO_SLV_FD_IM1D_NS_BEGIN


/**
 *  This solver is responsible for one homogenous rectangular area in 1D space.
 *  Only data model is changed comparing to the 2D implementation.
 *
 *  TODO: TimeStep for me is not clear... Why there is no /2 in 2D?
 */
class AreaSubSolver : public BIO_SLV_FD_IM2D_NS::AreaSubSolver
{

public:

    /**
     *  Constructor.
     */
    AreaSubSolver(
        BIO_SLV_FD_IM2D_NS::Solver* solver,
        int positionH,
        int positionV,
        BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
        BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer* fdAnalyzer
    );

    /**
     *  Destructor.
     */
    virtual ~AreaSubSolver();

    virtual void solveVerticalForward();

    virtual void solveVerticalBackward();


protected:

    virtual double**** createData();
    virtual void deleteData(double**** data);


};



BIO_SLV_FD_IM1D_NS_END

#endif
