#ifndef BIO_SLV_ITransducer_HXX
#define BIO_SLV_ITransducer_HXX
#include "../../biosensor.hxx"
BIO_SLV_NS_BEGIN


/**
 *  Transducer interface.
 */
class ITransducer
{
public:
    virtual ~ITransducer()
    {
        //  Empty virtual destructor.
    }

    /**
     *  Returns output of the transducer.
     */
    virtual double getOutput() = 0;

};



BIO_SLV_NS_END
#endif
