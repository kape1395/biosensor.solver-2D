#ifndef BIO_TRD_AmperometricElectrode2DOnBound_HXX
#define BIO_TRD_AmperometricElectrode2DOnBound_HXX
#include "../../biosensor.hxx"
#include "../slv/ISolver.hxx"
#include "../slv/ITransducer.hxx"
#include "../cfg/BoundAnalyzer.hxx"
#include "../dm/IDataModel.hxx"
#include "../dm/IGrid2D.hxx"
#include "../dm/ICursor2D.hxx"
#include "../dm/IComposite2D.hxx"
#include "../dm/ISegmentSplit.hxx"
#include <biosensor-xml.hxx>
#include <string>
#include <vector>
BIO_TRD_NS_BEGIN


/**
 *  Transducer: AmperometricElectrode.
 */
class AmperometricElectrode2DOnBound : public BIO_SLV_NS::ITransducer
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
    AmperometricElectrode2DOnBound(
        BIO_SLV_NS::ISolver* solver,
        BIO_XML_MODEL_NS::BoundName& boundName,
        BIO_XML_MODEL_NS::SubstanceName& substanceName
    );

    virtual ~AmperometricElectrode2DOnBound();

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



BIO_TRD_NS_END
#endif
