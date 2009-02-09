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
    static const double CONST_n_e  = 2.0;
    static const double CONST_F    = 96485.0;

private:
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer;
    BIO_CFG_NS::BoundAnalyzer* boundAnalyzer;
    BIO_DM_NS::IComposite2D* dataModel;
    BIO_XML_MODEL_NS::BoundName boundName;
    BIO_XML_MODEL_NS::SubstanceName substanceName;
    int substanceIndex;


    /**
     *
     */
    class BoundIntegrator
    {
    private:
        BIO_DM_NS::IGrid2D* area;
        BIO_DM_NS::ICursor2D* cursor0;  // Cursor on bound
        BIO_DM_NS::ICursor2D* cursor1;  // Cursor on next line at bound.
        BIO_CFG_NS::BoundAnalyzer::AreaSide side;
        double D;           //!<  Diffusion coefficient
        bool cylindrical;   //!<  Coordiname system is cylidrical.
        bool horizontal;    //!<  Is this bound horizontal?
        BIO_DM_NS::ISegmentSplit* pointsParallel;
        BIO_DM_NS::ISegmentSplit* pointsPerpendicular;
        double perpendicularStepSize;
        double integrationLinePosition;

    public:
        BoundIntegrator(
            BIO_DM_NS::IGrid2D* area,
            BIO_CFG_NS::BoundAnalyzer::AreaSide side,
            double diffusion,
            bool cylindricalCoordinates
        );
        ~BoundIntegrator();
        double integrate(int substanceIndex);

    private:
        void goToStart();
        void goToNext();
        double getIntagrationElement(int substanceIndex, int pointIndex);
    };
    std::vector<BoundIntegrator*> bounds;

public:
    AmperometricElectrode2DOnBound(
        BIO_SLV_NS::ISolver* solver,
        BIO_XML_MODEL_NS::BoundName& boundName,
        BIO_XML_MODEL_NS::SubstanceName& substanceName
    );

    virtual ~AmperometricElectrode2DOnBound();

    /**
     *  Returns current density, generated by the biosensor.
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
