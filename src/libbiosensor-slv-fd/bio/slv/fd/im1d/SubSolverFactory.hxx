#ifndef BIO_SLV_FD_IM1D_SubSolverFactory_HXX
#define BIO_SLV_FD_IM1D_SubSolverFactory_HXX
#include "../../../../biosensor-slv-fd.hxx"

#include "../im2d/ISubSolverFactory.hxx"
#include "../im2d/Solver.hxx"
#include "../im2d/AreaSubSolver.hxx"

BIO_SLV_FD_IM1D_NS_BEGIN


class SubSolverFactory : public BIO_SLV_FD_IM2D_NS::ISubSolverFactory
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

    virtual BIO_SLV_FD_IM2D_NS::AreaSubSolver* createAreaSubSolver(
        BIO_SLV_FD_IM2D_NS::Solver* solver,
        int h, int v
    );

};

BIO_SLV_FD_IM1D_NS_END

#endif
