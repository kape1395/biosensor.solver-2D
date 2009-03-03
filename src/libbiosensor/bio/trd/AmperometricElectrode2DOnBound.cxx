#include "AmperometricElectrode2DOnBound.hxx"
#include "../Exception.hxx"
#include "../dm/ConstantSegmentSplit.hxx"
#include "../slv/IIterativeSolver.hxx"


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::AmperometricElectrode2DOnBound::AmperometricElectrode2DOnBound(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::BoundName& boundName,
    BIO_XML_MODEL_NS::SubstanceName& substanceName
)
{
    if (!dynamic_cast<BIO_SLV_NS::IIterativeSolver*>(solver))
        throw Exception("AmperometricElectrode: Solver must implement IIterativeSolver.");

    if (!(dataModel = dynamic_cast<BIO_DM_NS::IComposite2D*>(solver->getData())))
        throw Exception("AmperometricElectrode: Data model must implement IComposite2D.");

    this->solver = solver;
    this->structAnalyzer = new BIO_CFG_NS::StructureAnalyzer(solver->getConfig());
    this->boundAnalyzer = new BIO_CFG_NS::BoundAnalyzer(structAnalyzer);
    this->boundName = boundName;
    this->substanceName = substanceName;
    this->substanceIndex = structAnalyzer->getSubstanceIndex(substanceName);


    for (int h = 0; h < dataModel->sizeH(); h++)
    {
        for (int v = 0; v < dataModel->sizeV(); v++)
        {
            addBoundCondition(h, v, BIO_CFG_NS::BoundAnalyzer::TOP);
            addBoundCondition(h, v, BIO_CFG_NS::BoundAnalyzer::RIGHT);
            addBoundCondition(h, v, BIO_CFG_NS::BoundAnalyzer::BOTTOM);
            addBoundCondition(h, v, BIO_CFG_NS::BoundAnalyzer::LEFT);
        }
    }
    if (bounds.size() == 0)
        throw Exception("AmperometricElectrode: No bound was found with specified name.");

    this->calculatedOutput = 0.0;
    this->calculatedOutputForStep = -1;
}


/* ************************************************************************** */
/* ************************************************************************** */
bool BIO_TRD_NS::AmperometricElectrode2DOnBound::addBoundCondition(
    int h,
    int v,
    BIO_CFG_NS::BoundAnalyzer::AreaSide side
)
{
    using namespace BIO_XML_NS::model::bound;
    using namespace BIO_CFG_NS;


    std::string* currentBoundName = boundAnalyzer->getBoundName(substanceIndex, h, v, side);
    if (!currentBoundName || boundName.compare(*currentBoundName) != 0)
        return false;

    bool nonConstAndHoriz =
        ((side == BIO_CFG_NS::BoundAnalyzer::TOP || side == BIO_CFG_NS::BoundAnalyzer::BOTTOM)) &&
        !dynamic_cast<BIO_DM_NS::ConstantSegmentSplit*>(dataModel->getBoundH(h, 0)->getPointPositions());
    bool nonConstAndVert =
        ((side == BIO_CFG_NS::BoundAnalyzer::RIGHT || side == BIO_CFG_NS::BoundAnalyzer::LEFT)) &&
        !dynamic_cast<BIO_DM_NS::ConstantSegmentSplit*>(dataModel->getBoundV(0, v)->getPointPositions());
    if (nonConstAndHoriz || nonConstAndVert)
        throw Exception("AmperometricElectrode: Only constant segment step sizes are supported.");

    bounds.push_back(new BoundIntegrator(
                         dataModel->getArea(h, v),
                         side,
                         structAnalyzer->getDiffusion(substanceIndex, h, v)->value(),
                         structAnalyzer->isCoordinateSystemCylindrical()
                     ));
    return true;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::AmperometricElectrode2DOnBound::~AmperometricElectrode2DOnBound()
{
    for (std::vector<BoundIntegrator*>::iterator b = bounds.begin(); b < bounds.end(); b++)
    {
        delete *b;
    }
    bounds.clear();
    delete boundAnalyzer;
    delete structAnalyzer;
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_TRD_NS::AmperometricElectrode2DOnBound::getOutput()
{
    BIO_SLV_NS::IIterativeSolver* iterative = dynamic_cast<BIO_SLV_NS::IIterativeSolver*>(solver);
    if (calculatedOutputForStep == iterative->getSolvedIterationCount())
    {
        return calculatedOutput;
    }

    double integralValue = 0.0;
    for (std::vector<BoundIntegrator*>::iterator bound = bounds.begin(); bound < bounds.end(); bound++)
    {
        integralValue += (*bound)->integrate(substanceIndex);
    }

    //
    //  Divide by surface.
    //
    if (structAnalyzer->isCoordinateSystemCylindrical())
    {
        //
        //   integrate by angle (\fi) and divide by an area of a circle:
        //      ((2 \pi) / (\pi r^2))
        //
        double cellRadius = structAnalyzer->getPointsH()[structAnalyzer->getPointsH().size() - 1]->value();
        integralValue *= 2.0 / (cellRadius * cellRadius);
    }
    else
    {
        //
        //  We are integrated all in one line, now just divide all by it`s lenght.
        //
        double areaWidth = structAnalyzer->getPointsH()[structAnalyzer->getPointsH().size() - 1]->value();
        integralValue /= areaWidth;
    }

    calculatedOutput = integralValue;
    calculatedOutputForStep = iterative->getSolvedIterationCount();

    return integralValue;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::AmperometricElectrode2DOnBound::BoundIntegrator::BoundIntegrator(
    BIO_DM_NS::IGrid2D* area,
    BIO_CFG_NS::BoundAnalyzer::AreaSide side,
    double diffusion,
    bool cylindricalCoordinates
)
{
    this->area = area;
    this->cursor0 = area->newGridCursor();
    this->cursor1 = area->newGridCursor();
    this->side = side;
    this->D = diffusion;
    this->cylindrical = cylindricalCoordinates;
    this->horizontal = (side == BIO_CFG_NS::BoundAnalyzer::TOP) || (side == BIO_CFG_NS::BoundAnalyzer::BOTTOM);
    this->pointsParallel = horizontal
                           ? area->getPointPositionsH()
                           : area->getPointPositionsV();
    this->pointsPerpendicular = horizontal
                                ? area->getPointPositionsV()
                                : area->getPointPositionsH();

    bool atStart = (side == BIO_CFG_NS::BoundAnalyzer::TOP) || (side == BIO_CFG_NS::BoundAnalyzer::LEFT);

    this->perpendicularStepSize = atStart
                                  ? pointsPerpendicular->getStepSize(0)
                                  : pointsPerpendicular->getStepSize(pointsPerpendicular->getStepCount() - 1);

    this->integrationLinePosition = atStart
                                    ? pointsPerpendicular->getPointPosition(0)
                                    : pointsPerpendicular->getPointPosition(pointsPerpendicular->getPointCount() - 1);
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::AmperometricElectrode2DOnBound::BoundIntegrator::~BoundIntegrator()
{
    delete this->cursor0;
    delete this->cursor1;
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_TRD_NS::AmperometricElectrode2DOnBound::BoundIntegrator::integrate(int substanceIndex)
{
    //std::cout << "DEBUG: integrate" << std::endl;
    goToStart();

    double sum = 0.0;
    double firstPointValue = getIntagrationElement(substanceIndex, 0);
    double lastPointValue = 0.0;
    for (int pointIndex = 0; cursor0->isValid(); goToNext(), pointIndex++)
    {
        sum += lastPointValue = getIntagrationElement(substanceIndex, pointIndex);
    }
    sum -= 0.5 * (lastPointValue + firstPointValue);  //  Substact half oh the first and last points.
    sum *= this->pointsParallel->getStepSize(0);    // XXX: Only correct for constant segment split.


    return AmperometricElectrode2DOnBound::CONST_F * AmperometricElectrode2DOnBound::CONST_n_e * D * sum;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_TRD_NS::AmperometricElectrode2DOnBound::BoundIntegrator::goToStart()
{
    switch (side)
    {
    case BIO_CFG_NS::BoundAnalyzer::TOP:
        cursor0->colStart();
        cursor1->colStart();
        cursor0->rowStart();
        cursor1->rowStart();
        cursor1->down();
        //std::cout << "DEBUG: goToStart(TOP)" << std::endl;
        break;
    case BIO_CFG_NS::BoundAnalyzer::RIGHT:
        cursor0->colStart();
        cursor1->colStart();
        cursor0->rowEnd();
        cursor1->rowEnd();
        cursor1->left();
        //std::cout << "DEBUG: goToStart(RIGHT)" << std::endl;
        break;
    case BIO_CFG_NS::BoundAnalyzer::BOTTOM:
        cursor0->colEnd();
        cursor1->colEnd();
        cursor0->rowStart();
        cursor1->rowStart();
        cursor1->top();
        //std::cout << "DEBUG: goToStart(BOTTOM)" << std::endl;
        break;
    case BIO_CFG_NS::BoundAnalyzer::LEFT:
        cursor0->colStart();
        cursor1->colStart();
        cursor0->rowStart();
        cursor1->rowStart();
        cursor1->right();
        //std::cout << "DEBUG: goToStart(LEFT)" << std::endl;
        break;
    default:
        throw Exception("Impossible");
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_TRD_NS::AmperometricElectrode2DOnBound::BoundIntegrator::goToNext()
{
    if (horizontal)
    {
        cursor0->right();
        cursor1->right();
    }
    else
    {
        cursor0->down();
        cursor1->down();
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_TRD_NS::AmperometricElectrode2DOnBound::BoundIntegrator::getIntagrationElement(int substanceIndex, int pointIndex)
{
    //std::cout << "DEBUG: a=" << pointIndex << std::endl;
    //
    //  Calculate derivate (f(b) - f(a) / distance)
    //
    double value = (
                       cursor1->getConcentrations()->getConcentration(substanceIndex) -
                       cursor0->getConcentrations()->getConcentration(substanceIndex)
                   )
                   / perpendicularStepSize;

    //
    //  Multiply by R in case of cylindrical coor...
    //
    if (cylindrical)
    {
        value *= horizontal
                 ? pointsParallel->getPointPosition(pointIndex)
                 : integrationLinePosition;
    }

    return value;
}


/* ************************************************************************** */
/* ************************************************************************** */


