#ifndef BIO_SLV_AmperometricElectrode_HXX
#define BIO_SLV_AmperometricElectrode_HXX
#include "ISolver.hxx"
#include "ITransducer.hxx"
#include "../../biosensor.hxx"
#include <string>
BIO_SLV_NS_BEGIN


/**
 *  Transducer: AmperometricElectrode.
 */
class AmperometricElectrode : public ITransducer
{
public:
    AmperometricElectrode(
        ISolver* solver,
        std::string& boundName,
        std::string& substanceName
    );

    virtual ~AmperometricElectrode();

    /**
     *  Returns output of the transducer.
     */
    virtual double getOutput();

};



BIO_SLV_NS_END
#endif
