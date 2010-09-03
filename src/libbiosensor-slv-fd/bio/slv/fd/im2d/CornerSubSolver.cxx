#include "CornerSubSolver.hxx"
#include <cmath>
#include <vector>
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
    LOG_DEBUG(LOGGER << "Constructing CornerSubSolver for position h=" << positionH << " v=" << positionV << " ...");
    substCount = structAnalyzer->getSubstances().size();
    for (unsigned s = 0; s < substCount; s++)
    {
        std::vector<BIO_DM_NS::ICursor1D*> substCursors;  // all cursors for a particular substance
        BIO_DM_NS::ICursor1D* cursor;                     // just temporary variable.

        if (boundTop && boundTop->getSubstanceConf(s))
        {
            cursor = boundTop->newGridCursor();
            cursor->end();
            substCursors.push_back(cursor);
            cursorsPlain.push_back(cursor);
        }
        if (boundRight && boundRight->getSubstanceConf(s))
        {
            cursor = boundRight->newGridCursor();
            cursor->start();
            substCursors.push_back(cursor);
            cursorsPlain.push_back(cursor);
        }
        if (boundBottom && boundBottom->getSubstanceConf(s))
        {
            cursor = boundBottom->newGridCursor();
            cursor->start();
            substCursors.push_back(cursor);
            cursorsPlain.push_back(cursor);
        }
        if (boundLeft && boundLeft->getSubstanceConf(s))
        {
            cursor = boundLeft->newGridCursor();
            cursor->end();
            substCursors.push_back(cursor);
            cursorsPlain.push_back(cursor);
        }
        cursors.push_back(substCursors);
        LOG_DEBUG(LOGGER << "Found " << substCursors.size() << " cursors for substance index=" << s);
    }

    applyInitialConditions(
        boundTop,
        boundRight,
        boundBottom,
        boundLeft
    );

    LOG_DEBUG(LOGGER << "Constructing CornerSubSolver for position h=" << positionH << " v=" << positionV << " ... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::CornerSubSolver::~CornerSubSolver()
{
    LOG_TRACE(LOGGER << "~CornerSubSolver()...");

    std::vector< BIO_DM_NS::ICursor1D* >::iterator it;
    for (it = cursorsPlain.begin(); it < cursorsPlain.end(); it++)
    {
        delete *it;
    }

    LOG_TRACE(LOGGER << "~CornerSubSolver()... Done");
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


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_SLV_FD_IM2D_NS::CornerSubSolver::getConcentration(int s)
{
    return cursors[s].size() > 0 ? cursors[s][0]->getConcentrations()->getConcentration(s) : NAN;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::CornerSubSolver::setConcentration(int s, double c)
{
    std::vector< BIO_DM_NS::ICursor1D* >::iterator it;
    for (it = cursors[s].begin(); it < cursors[s].end(); it++)
    {
        (*it)->getConcentrations()->setConcentration(s, c);
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::CornerSubSolver::applyInitialConditions(
    BoundSubSolver* boundTop,
    BoundSubSolver* boundRight,
    BoundSubSolver* boundBottom,
    BoundSubSolver* boundLeft
)
{
    //
    //  That's more a workaround, than a solution.
    //
    if (boundTop)
        boundTop->applyInitialValues();
    if (boundRight)
        boundRight->applyInitialValues();
    if (boundBottom)
        boundBottom->applyInitialValues();
    if (boundLeft)
        boundLeft->applyInitialValues();

    if (boundTop)
        boundTop->applyInitialValues();
    if (boundRight)
        boundRight->applyInitialValues();
    if (boundBottom)
        boundBottom->applyInitialValues();
    if (boundLeft)
        boundLeft->applyInitialValues();
}

/* ************************************************************************** */
/* ************************************************************************** */
