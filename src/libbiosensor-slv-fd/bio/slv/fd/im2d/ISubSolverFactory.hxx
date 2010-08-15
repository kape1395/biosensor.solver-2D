#ifndef BIO_SLV_FD_IM2D_ISubSolverFactory_HXX
#define BIO_SLV_FD_IM2D_ISubSolverFactory_HXX
#include "../../../../biosensor-slv-fd.hxx"

BIO_SLV_FD_IM2D_NS_BEGIN
class ISubSolverFactory;
BIO_SLV_FD_IM2D_NS_END

#include "Solver.hxx"
#include "AreaSubSolver.hxx"

BIO_SLV_FD_IM2D_NS_BEGIN


class ISubSolverFactory
{
public:

    virtual ~ISubSolverFactory()
    {
        //  nothing.
    }

    virtual AreaSubSolver* createAreaSubSolver(
        Solver* solver,
        int h, int v
    ) = 0;

};

BIO_SLV_FD_IM2D_NS_END

#endif
