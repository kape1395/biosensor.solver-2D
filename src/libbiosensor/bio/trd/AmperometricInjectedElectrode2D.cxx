#include "AmperometricInjectedElectrode2D.hxx"
#include "../Exception.hxx"
#include "../dm/ConstantSegmentSplit.hxx"


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::AmperometricInjectedElectrode2D::AmperometricInjectedElectrode2D(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::MediumName& mediumName,
    BIO_XML_MODEL_NS::SubstanceName& substanceName
)
{
    // TODO: AmperometricInjectedElectrode2D
    /*
    if ((dataModel == dynamic_cast<BIO_DM_NS::IComposite2D*>(solver->getData())) == 0)
    {
        throw Exception("InjectedElectrode: Data model must implement IComposite2D.");
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
     */
}




/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::AmperometricInjectedElectrode2D::~AmperometricInjectedElectrode2D()
{
    // TODO: Implement ~AmperometricInjectedElectrode2D
    /*
    for (std::vector<BoundAddress*>::iterator b = bounds.begin(); b < bounds.end(); b++)
    {
        delete *b;
    }
    bounds.clear();
     */
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_TRD_NS::AmperometricInjectedElectrode2D::getOutput()
{
    //  TODO: Implement getOutput()
    return 0.0;
}

/* ************************************************************************** */
/* ************************************************************************** */
