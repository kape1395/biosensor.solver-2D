#include "SubSolverFactory.hxx"
#include "AreaSubSolver.hxx"

BIO_SLV_FD_IM2D_NS::AreaSubSolver*
BIO_SLV_FD_IM1D_NS::SubSolverFactory::createAreaSubSolver(
    BIO_SLV_FD_IM2D_NS::Solver* solver,
    int h, int v
)
{
    AreaSubSolver* ass = new BIO_SLV_FD_IM1D_NS::AreaSubSolver(
        solver, h, v,
        solver->getStructAnalyzer(),
        solver->getFDAnalyzer()
    );
    ass->initialize();
    return ass; // ;)
}
