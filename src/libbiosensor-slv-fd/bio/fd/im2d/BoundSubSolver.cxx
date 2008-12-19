#include "BoundSubSolver.hxx"


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::BoundSubSolver::BoundSubSolver(
    Solver* solver,
    int positionH,
    int positionV,
    bool horizontal,
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
    BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer* fdAnalyzer,
    BIO_CFG_NS::BoundAnalyzer* boundAnalyzer
) :  log(log4cxx::Logger::getLogger("libbiosensor-slv-fd::im2d::BoundSubSolver"))
{
    LOG4CXX_DEBUG(log, "BoundSubSolver()...");
    this->solver = solver;
    this->positionH = positionH;
    this->positionV = positionV;
    this->horizontal = horizontal;
    this->structAnalyzer = structAnalyzer;
    this->fdAnalyzer = fdAnalyzer;
    this->boundAnalyzer = boundAnalyzer;



    LOG4CXX_DEBUG(log, "BoundSubSolver()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::BoundSubSolver::~BoundSubSolver()
{
    LOG4CXX_DEBUG(log, "~BoundSubSolver()");
    //  FIXME: Implement
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::BoundSubSolver::solveThroughForward()
{
    //  FIXME: Implement
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::BoundSubSolver::solveThroughBackward()
{
    //  FIXME: Implement
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::BoundSubSolver::solveAlongForward()
{
    //  FIXME: Implement
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::BoundSubSolver::solveAlongBackward()
{
    //  FIXME: Implement
}


/* ************************************************************************** */
/* ************************************************************************** */
