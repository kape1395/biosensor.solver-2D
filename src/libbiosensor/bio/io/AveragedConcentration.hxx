#ifndef BIO_IO_AveragedConcentration_HXX
#define BIO_IO_AveragedConcentration_HXX
#include "../../biosensor.hxx"
#include "../slv/ISolver.hxx"
#include "IOutput.hxx"
#include "IContext.hxx"
#include <biosensor-xml.hxx>
#include <string>
#include <ostream>
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

    std::string name;
    BIO_SLV_NS::ISolver* solver;
    BIO_IO_NS::IContext* context;
    BIO_XML_MODEL_NS::MediumName* medium;

    std::ostream* output;

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

};



BIO_IO_NS_END
#endif
