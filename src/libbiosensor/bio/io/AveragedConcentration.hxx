#ifndef BIO_IO_AveragedConcentration_HXX
#define BIO_IO_AveragedConcentration_HXX
#include "../../biosensor.hxx"
#include "../cfg/StructureAnalyzer.hxx"
#include "../slv/ISolver.hxx"
#include "../trd/IntegralOverArea.hxx"
#include "IOutput.hxx"
#include "IContext.hxx"
#include <biosensor-xml.hxx>
#include <string>
#include <ostream>
#include <vector>
BIO_IO_NS_BEGIN


/**
 *  An output generator for an averaged concentration.
 *  Can generate output for specific medium or for whole biosensor.
 *
 *  \author k.petrauskas
 */
class AveragedConcentration : public BIO_IO_NS::IOutput
{
private:
    typedef std::vector<BIO_TRD_NS::IntegralOverArea*> Integrals;
    typedef std::vector<BIO_XML_MODEL_NS::Substance*> Substances;
    std::string name;
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer;
    BIO_SLV_NS::ISolver* solver;
    BIO_IO_NS::IContext* context;
    BIO_XML_MODEL_NS::MediumName* medium;

    std::ostream* output;

    Substances substances;
    Integrals integrals;

public:
    /**
     *  Constructor.
     */
    AveragedConcentration(
        std::string& name,
        BIO_SLV_NS::ISolver* solver,
        BIO_IO_NS::IContext* context,
        BIO_XML_MODEL_NS::MediumName* medium
    );

    /**
     *  Destructor.
     */
    virtual ~AveragedConcentration();

    /**
     *  EventListener.
     */
    virtual void solveEventOccured();

    /**
     *  Reset listener's internal state.
     */
    virtual void reset()
    {
        //  Nothing to reset.
    }
    
};



BIO_IO_NS_END
#endif
