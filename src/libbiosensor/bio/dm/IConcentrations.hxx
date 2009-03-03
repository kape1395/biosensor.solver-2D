#ifndef BIO_DM_IConcentrations_HXX
#define BIO_DM_IConcentrations_HXX
#include "../../biosensor.hxx"
BIO_DM_NS_BEGIN


/**
 *  Interface to access concentration values.
 */
class IConcentrations
{
public:

    virtual ~IConcentrations()
    {
        //  Empty virtual destructor.
    }

    /**
     *  Returns concentration of the substance with index <code>substanceNr</code>.
     */
    virtual double getConcentration(int substanceNr) = 0;

};

BIO_DM_NS_END
#endif
