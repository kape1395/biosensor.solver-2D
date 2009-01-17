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

    /**
     *  Returns concentration of the substance with index <code>substanceNr</code>.
     */
    virtual double operator [] (int substanceNr) = 0;

};

BIO_DM_NS_END
#endif
