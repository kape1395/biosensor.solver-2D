#include "BoundSubSolver.hxx"
#include "ConstantCondition.hxx"
#include "WallCondition.hxx"
#include "MergeCondition.hxx"
#include <bio/Exception.hxx>


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::BoundSubSolver::BoundSubSolver(
    Solver* solver,
    int positionH,
    int positionV,
    bool horizontal,
    AreaSubSolver* areaPrev,
    AreaSubSolver* areaNext,
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

    for (int s = 0; s < structAnalyzer->getSubstances().size(); s++)
    {
        if (horizontal)
        {
            if (positionV > 0)
            {
                createBoundCondition(
                    boundAnalyzer->getBound(s, positionH, positionV - 1, boundAnalyzer->BOTTOM),
                    areaPrev, areaNext, s, false
                );
            }
            if (positionV < structAnalyzer->getPointsV().size() - 1)
            {
                createBoundCondition(
                    boundAnalyzer->getBound(s, positionH, positionV, boundAnalyzer->TOP),
                    areaPrev, areaNext, s, true
                );
            }
        }
        else // vertical
        {
            if (positionH > 0)
            {
                createBoundCondition(
                    boundAnalyzer->getBound(s, positionH - 1, positionV, boundAnalyzer->RIGHT),
                    areaPrev, areaNext, s, false
                );
            }
            if (positionH < structAnalyzer->getPointsH().size() - 1)
            {
                createBoundCondition(
                    boundAnalyzer->getBound(s, positionH, positionV, boundAnalyzer->LEFT),
                    areaPrev, areaNext, s, true
                );
            }
        }
    }

    LOG4CXX_DEBUG(log, "BoundSubSolver()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::BoundSubSolver::~BoundSubSolver()
{
    LOG4CXX_DEBUG(log, "~BoundSubSolver()...");

    for (BCIterator bc = boundConditions.begin(); bc < boundConditions.end(); bc++)
    {
        delete *bc;
    }
    boundConditions.clear();

    LOG4CXX_DEBUG(log, "~BoundSubSolver()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::BoundSubSolver::solveThroughForward()
{
    for (BCIterator bc = boundConditions.begin(); bc < boundConditions.end(); bc++)
    {
        (*bc)->solveThroughForward();
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::BoundSubSolver::solveThroughBackward()
{
    for (BCIterator bc = boundConditions.begin(); bc < boundConditions.end(); bc++)
    {
        (*bc)->solveThroughBackward();
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::BoundSubSolver::solveAlongForward()
{
    for (BCIterator bc = boundConditions.begin(); bc < boundConditions.end(); bc++)
    {
        (*bc)->solveAlongForward();
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::BoundSubSolver::solveAlongBackward()
{
    for (BCIterator bc = boundConditions.begin(); bc < boundConditions.end(); bc++)
    {
        (*bc)->solveAlongBackward();
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::BoundSubSolver::applyInitialValues()
{
    for (BCIterator bc = boundConditions.begin(); bc < boundConditions.end(); bc++)
    {
        (*bc)->applyInitialValues();
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::BoundSubSolver::createBoundCondition(
    BIO_XML_NS::model::BoundSubstance * boundSubstance,
    AreaSubSolver* areaPrev,
    AreaSubSolver* areaNext,
    int substance,
    bool atStart
)
{
    IBoundCondition *bc = 0;
    if (dynamic_cast<BIO_XML_NS::model::bound::Constant*>(boundSubstance) != 0)
    {
        BIO_XML_NS::model::bound::Constant* bsConst = dynamic_cast<BIO_XML_NS::model::bound::Constant*>(boundSubstance);
        bc = new ConstantCondition(
            (atStart ? areaNext : areaPrev)->getEdgeData(substance, horizontal, atStart),
            structAnalyzer->getSymbol(bsConst->concentration())->value(),
            atStart
        );

    }
    else if (dynamic_cast<BIO_XML_NS::model::bound::Wall*>(boundSubstance) != 0)
    {
        bc = new WallCondition(
            (atStart ? areaNext : areaPrev)->getEdgeData(substance, horizontal, atStart),
            atStart
        );
    }
    else if (dynamic_cast<BIO_XML_NS::model::bound::Merge*>(boundSubstance) != 0)
    {
        if (atStart)    // This BC is for both sides so only one should be created.
        {
            bc = new MergeCondition(
                areaPrev->getEdgeData(substance, horizontal, false),    // atEnd
                areaNext->getEdgeData(substance, horizontal, true),     // atStart
                structAnalyzer->getDiffusion(substance, areaPrev->getPositionH(), areaPrev->getPositionV())->value(),
                structAnalyzer->getDiffusion(substance, areaNext->getPositionH(), areaNext->getPositionV())->value()
            );
        }
    }
    else if (dynamic_cast<BIO_XML_NS::model::bound::Null*>(boundSubstance) != 0)
    {
        // nothing
        bc = 0;
    }
    else
    {
        throw BIO_NS::Exception("Unknown bound condition");
    }

    if (bc != 0)
    {
        boundConditions.push_back(bc);
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
