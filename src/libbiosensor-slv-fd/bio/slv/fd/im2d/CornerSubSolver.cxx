#include "CornerSubSolver.hxx"
#include <cmath>


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::CornerSubSolver::CornerSubSolver(
    int positionH,
    int positionV,
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
    BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer* fdAnalyzer
) :
        log(log4cxx::Logger::getLogger("libbiosensor-slv-fd.im2d.CornerSubSolver")),
        BIO_DM_NS::IConcentrations()
{
    LOG4CXX_DEBUG(log, "CornerSubSolver()");
    //  FIXME: Implement CornerSubSolver(
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::CornerSubSolver::~CornerSubSolver()
{
    LOG4CXX_DEBUG(log, "~CornerSubSolver()");
    //  FIXME: Implement ~CornerSubSolver()
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::CornerSubSolver::solveForward()
{
    //  FIXME: Implement solveForward()
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::CornerSubSolver::solveBackward()
{
    //  FIXME: Implement solveBackward()
}

double BIO_SLV_FD_IM2D_NS::CornerSubSolver::getConcentration(int s)
{
    //  FIXME: Implement getConcentration(int s)
    return NAN;
}

/* ************************************************************************** */
/* ************************************************************************** */
