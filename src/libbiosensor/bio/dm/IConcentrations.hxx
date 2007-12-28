#ifndef BIO_DM_IConcentrations_HXX
#define BIO_DM_IConcentrations_HXX
#include "../../biosensor.hxx"
BIO_DM_NS_BEGIN


/**
 *  Interface to access concentration values.
 *  TODO: Gal cia ideti ir getCount() ?
 */
class IConcentrations
{
public:
    virtual double operator [] (int substanceNr) = 0;
};



BIO_DM_NS_END

#endif
