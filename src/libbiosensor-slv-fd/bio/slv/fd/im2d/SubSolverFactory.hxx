#ifndef BIO_SLV_FD_IM2D_SubSolverFactory_HXX
#define BIO_SLV_FD_IM2D_SubSolverFactory_HXX
#include "../../../../biosensor-slv-fd.hxx"

#include "ISubSolverFactory.hxx"
#include "Solver.hxx"
#include "AreaSubSolver.hxx"

BIO_SLV_FD_IM2D_NS_BEGIN


class SubSolverFactory : public ISubSolverFactory
{
public:

    SubSolverFactory()
    {
        // Nothing.
    }

    virtual ~SubSolverFactory()
    {
        //  nothing.
    }

    virtual AreaSubSolver* createAreaSubSolver(
        Solver* solver,
        int h, int v
    );

};

BIO_SLV_FD_IM2D_NS_END

#endif
