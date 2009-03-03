#include "CornerSubSolver.hxx"
#include <cmath>
#include <bio/Logging.hxx>
#define LOGGER "libbiosensor-slv-fd::im2d::CornerSubSolver: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::CornerSubSolver::CornerSubSolver(
    Solver* solver,
    int positionH,
    int positionV,
    BoundSubSolver* boundTop,
    BoundSubSolver* boundRight,
    BoundSubSolver* boundBottom,
    BoundSubSolver* boundLeft,
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
    BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer* fdAnalyzer
) : BIO_DM_NS::IConcentrations()
{
    unsigned substCount = structAnalyzer->getSubstances().size();
    cursors = new BIO_DM_NS::ICursor1D*[substCount];
    for (unsigned s = 0; s < substCount; s++)
    {
        if (boundTop && boundTop->getSubstanceConf(s))
        {
            cursors[s] = boundTop->newGridCursor();
            cursors[s]->end();
        }
        else if (boundRight && boundRight->getSubstanceConf(s))
        {
            cursors[s] = boundRight->newGridCursor();
            cursors[s]->start();
        }
        else if (boundBottom && boundBottom->getSubstanceConf(s))
        {
            cursors[s] = boundBottom->newGridCursor();
            cursors[s]->start();
        }
        else if (boundLeft && boundLeft->getSubstanceConf(s))
        {
            cursors[s] = boundLeft->newGridCursor();
            cursors[s]->end();
        }
        else
        {
            cursors[s] = 0;
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::CornerSubSolver::~CornerSubSolver()
{
    for (unsigned s = 0; s < structAnalyzer->getSubstances().size(); s++)
    {
        if (cursors[s])
            delete cursors[s];
    }
    delete [] cursors;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::CornerSubSolver::solveForward()
{
    //  Bound conditions are solving points at corners too.
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::CornerSubSolver::solveBackward()
{
    //  Bound conditions are solving points at corners too.
}

double BIO_SLV_FD_IM2D_NS::CornerSubSolver::getConcentration(int s)
{
    return cursors[s] ? (*cursors[s]->getConcentrations())[s] : NAN;
}

/* ************************************************************************** */
/* ************************************************************************** */
