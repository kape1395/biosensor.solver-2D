#include "AmperometricElectrode2DOnBound.hxx"
#include "../Exception.hxx"
#include "../dm/ConstantSegmentSplit.hxx"


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::AmperometricElectrode2DOnBound::BoundAddress::BoundAddress(
    BIO_DM_NS::IGrid2D* area,
    BIO_CFG_NS::BoundAnalyzer::AreaSide side
)
{
    this->area = area;
    this->cursor = area->newGridCursor();
    this->side = side;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::AmperometricElectrode2DOnBound::BoundAddress::~BoundAddress()
{
    delete this->cursor;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::AmperometricElectrode2DOnBound::AmperometricElectrode2DOnBound(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::BoundName& boundName,
    BIO_XML_MODEL_NS::SubstanceName& substanceName
)
{
    if ((dataModel == dynamic_cast<BIO_DM_NS::IComposite2D*>(solver->getData())) == 0)
    {
        throw Exception("AmperometricElectrode: Data model must implement IComposite2D.");
    }
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
    {
        throw Exception("AmperometricElectrode: No bound was found with specified name.");
    }
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

    if (boundName != *boundAnalyzer->getBoundName(h, v, side))
        return false;

    Constant* bound = dynamic_cast<Constant*>(boundAnalyzer->getBoundForSubstance(substanceIndex, h, v, side));
    if (!bound)
        throw Exception("AmperometricElectrode: only Constant bound condition is supported");

    if (!structAnalyzer->getSymbol(bound->concentration())->value() != 0.0)
        throw Exception("AmperometricElectrode: constant condition must have 0 as value.");

    bool nonConstAndHoriz =
        ((side == BIO_CFG_NS::BoundAnalyzer::TOP || side == BIO_CFG_NS::BoundAnalyzer::BOTTOM)) &&
        !dynamic_cast<BIO_DM_NS::ConstantSegmentSplit*>(dataModel->getBoundH(h, 0)->getPointPositions());
    bool nonConstAndVert =
        ((side == BIO_CFG_NS::BoundAnalyzer::RIGHT || side == BIO_CFG_NS::BoundAnalyzer::LEFT)) &&
        !dynamic_cast<BIO_DM_NS::ConstantSegmentSplit*>(dataModel->getBoundV(0, v)->getPointPositions());
    if (nonConstAndHoriz || nonConstAndVert)
        throw Exception("AmperometricElectrode: Only constant segment step sizes are supported.");

    bounds.push_back(new BoundAddress(dataModel->getArea(h, v), side));
    return true;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::AmperometricElectrode2DOnBound::~AmperometricElectrode2DOnBound()
{
    for (std::vector<BoundAddress*>::iterator b = bounds.begin(); b < bounds.end(); b++)
    {
        delete *b;
    }
    bounds.clear();
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_TRD_NS::AmperometricElectrode2DOnBound::getOutput()
{
    //  TODO: Implement getOutput()
    return 0.0;
}

/* ************************************************************************** */
/* ************************************************************************** */
