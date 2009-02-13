#include "BoundSubSolver.hxx"
#include "ConstantCondition.hxx"
#include "WallCondition.hxx"
#include "MergeCondition.hxx"
#include <bio/Logging.hxx>
#include <bio/Exception.hxx>
#include <cmath>
#include <string>
#define LOGGER "libbiosensor-slv-fd::im2d::BoundSubSolver: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::BoundSubSolver::BoundSubSolver(
    Solver* solver,
    unsigned positionH,
    unsigned positionV,
    bool horizontal,
    AreaSubSolver* areaPrev,
    AreaSubSolver* areaNext,
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
    BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer* fdAnalyzer,
    BIO_CFG_NS::BoundAnalyzer* boundAnalyzer
)
{
    LOG_DEBUG(LOGGER << "BoundSubSolver(posH=" << positionH << ", posV=" << positionV << ", horiz=" << horizontal << ")...");

    this->solver = solver;
    this->positionH = positionH;
    this->positionV = positionV;
    this->horizontal = horizontal;
    this->structAnalyzer = structAnalyzer;
    this->fdAnalyzer = fdAnalyzer;
    this->boundAnalyzer = boundAnalyzer;
    this->segmentSplit = horizontal
                         ? fdAnalyzer->getAxisPartSegmentSplitH(positionH)
                         : fdAnalyzer->getAxisPartSegmentSplitV(positionV)
                         ;

    substanceToBCMap = new IBoundCondition*[structAnalyzer->getSubstances().size()];

    for (unsigned s = 0; s < structAnalyzer->getSubstances().size(); s++)
    {
        substanceToBCMap[s] = 0;

        if (horizontal)
        {
            if (positionV > 0)
            {
                createBoundCondition(
                    boundAnalyzer->getBoundForSubstance(s, positionH, positionV - 1, boundAnalyzer->BOTTOM),
                    areaPrev, areaNext, s, false
                );
            }
            if (positionV < structAnalyzer->getPointsV().size() - 1)
            {
                createBoundCondition(
                    boundAnalyzer->getBoundForSubstance(s, positionH, positionV, boundAnalyzer->TOP),
                    areaPrev, areaNext, s, true
                );
            }
        }
        else // vertical
        {
            if (positionH > 0)
            {
                createBoundCondition(
                    boundAnalyzer->getBoundForSubstance(s, positionH - 1, positionV, boundAnalyzer->RIGHT),
                    areaPrev, areaNext, s, false
                );
            }
            if (positionH < structAnalyzer->getPointsH().size() - 1)
            {
                createBoundCondition(
                    boundAnalyzer->getBoundForSubstance(s, positionH, positionV, boundAnalyzer->LEFT),
                    areaPrev, areaNext, s, true
                );
            }
        }
    }

    LOG_DEBUG(LOGGER << "BoundSubSolver()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::BoundSubSolver::~BoundSubSolver()
{
    LOG_DEBUG(LOGGER << "~BoundSubSolver()...");

    for (BCIterator bc = boundConditions.begin(); bc < boundConditions.end(); bc++)
    {
        delete *bc;
    }
    boundConditions.clear();

    delete [] substanceToBCMap;

    LOG_DEBUG(LOGGER << "~BoundSubSolver()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::BoundSubSolver::solveThroughForward()
{
    LOG_TRACE(LOGGER << "solveThroughForward()...");
    for (BCIterator bc = boundConditions.begin(); bc < boundConditions.end(); bc++)
    {
        (*bc)->solveThroughForward();
    }
    LOG_TRACE(LOGGER << "solveThroughForward()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::BoundSubSolver::solveThroughBackward()
{
    LOG_TRACE(LOGGER << "solveThroughBackward()...");
    for (BCIterator bc = boundConditions.begin(); bc < boundConditions.end(); bc++)
    {
        (*bc)->solveThroughBackward();
    }
    LOG_TRACE(LOGGER << "solveThroughBackward()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::BoundSubSolver::solveAlongForward()
{
    LOG_TRACE(LOGGER << "solveAlongForward()...");
    for (BCIterator bc = boundConditions.begin(); bc < boundConditions.end(); bc++)
    {
        (*bc)->solveAlongForward();
    }
    LOG_TRACE(LOGGER << "solveAlongForward()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::BoundSubSolver::solveAlongBackward()
{
    LOG_TRACE(LOGGER << "solveAlongBackward()...");
    for (BCIterator bc = boundConditions.begin(); bc < boundConditions.end(); bc++)
    {
        (*bc)->solveAlongBackward();
    }
    LOG_TRACE(LOGGER << "solveAlongBackward()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::BoundSubSolver::applyInitialValues()
{
    LOG_DEBUG(LOGGER << "applyInitialValues()...");
    for (BCIterator bc = boundConditions.begin(); bc < boundConditions.end(); bc++)
    {
        (*bc)->applyInitialValues();
    }
    LOG_DEBUG(LOGGER << "applyInitialValues()... Done");
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

        if (substanceToBCMap[substance] == 0)
        {
            substanceToBCMap[substance] = bc;
        }
        else
        {
            throw Exception("Two bound conditions on one edge for one substance is not supported.");
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
int BIO_SLV_FD_IM2D_NS::BoundSubSolver::getSubstanceCount()
{
    return structAnalyzer->getSubstances().size();
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_XML_NS::model::Substance* BIO_SLV_FD_IM2D_NS::BoundSubSolver::getSubstanceConf(int index)
{
    return (substanceToBCMap[index])
           ? structAnalyzer->getSubstances()[index]
           : 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_DM_NS::ISegmentSplit* BIO_SLV_FD_IM2D_NS::BoundSubSolver::getPointPositions()
{
    return segmentSplit;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_DM_NS::ICursor1D* BIO_SLV_FD_IM2D_NS::BoundSubSolver::newGridCursor()
{
    return new Cursor(this);
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_SLV_FD_IM2D_NS::BoundSubSolver::getConcentration(int x, int s)
{
    return (substanceToBCMap[s])
           ? substanceToBCMap[s]->getConcentration(x)
           : NAN;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::BoundSubSolver::Cursor::Cursor(BoundSubSolver* subSolver)
{
    this->subSolver = subSolver;
    this->position = 0;
    this->pointCount = subSolver->getPointPositions()->getPointCount();
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::BoundSubSolver::Cursor::~Cursor()
{
    // Nothing to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::BoundSubSolver::Cursor::prev()
{
    position--;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::BoundSubSolver::Cursor::next()
{
    position++;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::BoundSubSolver::Cursor::start()
{
    position = 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::BoundSubSolver::Cursor::end()
{
    position = pointCount - 1;
}


/* ************************************************************************** */
/* ************************************************************************** */
bool BIO_SLV_FD_IM2D_NS::BoundSubSolver::Cursor::isValid()
{
    return position >= 0 && position < pointCount;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_DM_NS::IConcentrations* BIO_SLV_FD_IM2D_NS::BoundSubSolver::Cursor::getConcentrations()
{
    if (!isValid())
    {
        return 0;
    }
    return this;
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_SLV_FD_IM2D_NS::BoundSubSolver::Cursor::operator[] (int substanceNr)
{
    return subSolver->getConcentration(position, substanceNr);
}


/* ************************************************************************** */
/* ************************************************************************** */
