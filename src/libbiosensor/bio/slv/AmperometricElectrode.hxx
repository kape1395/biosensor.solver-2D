#ifndef BIO_SLV_AmperometricElectrode_HXX
#define BIO_SLV_AmperometricElectrode_HXX
#include "ISolver.hxx"
#include "ITransducer.hxx"
#include "../../biosensor.hxx"
#include "../cfg/BoundAnalyzer.hxx"
#include "../dm/IDataModel.hxx"
#include "../dm/IGrid2D.hxx"
#include "../dm/ICursor2D.hxx"
#include "../dm/IComposite2D.hxx"
#include "../dm/ISegmentSplit.hxx"
#include <biosensor-xml.hxx>
#include <string>
#include <vector>
BIO_SLV_NS_BEGIN


/**
 *  Transducer: AmperometricElectrode.
 */
class AmperometricElectrode : public ITransducer
{
private:
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer;
    BIO_CFG_NS::BoundAnalyzer* boundAnalyzer;
    BIO_DM_NS::IComposite2D* dataModel;
    BIO_XML_MODEL_NS::BoundName boundName;
    BIO_XML_MODEL_NS::SubstanceName substanceName;
    int substanceIndex;

    struct BoundAddress
    {
        BoundAddress(BIO_DM_NS::IGrid2D* area, BIO_CFG_NS::BoundAnalyzer::AreaSide side);
        ~BoundAddress();
        BIO_DM_NS::IGrid2D* area;
        BIO_DM_NS::ICursor2D* cursor;
        BIO_CFG_NS::BoundAnalyzer::AreaSide side;
    };
    std::vector<BoundAddress*> bounds;

public:
    AmperometricElectrode(
        ISolver* solver,
        BIO_XML_MODEL_NS::BoundName& boundName,
        BIO_XML_MODEL_NS::SubstanceName& substanceName
    );

    virtual ~AmperometricElectrode();

    /**
     *  Returns output of the transducer.
     */
    virtual double getOutput();

private:

    /**
     *  Checks if bound belongs to this electrode, validates needed things and
     *  adds it tho the #bounds list.
     *
     *  \return true, if bound is added to #bounds list.
     */
    bool addBoundCondition(int h, int v, BIO_CFG_NS::BoundAnalyzer::AreaSide side);

};



BIO_SLV_NS_END
#endif
