#ifndef BIO_CFG_IConfigTransformer_HXX
#define BIO_CFG_IConfigTransformer_HXX
#include "../../biosensor.hxx"
#include <biosensor-xml.hxx>
BIO_CFG_NS_BEGIN


/**
 *  Implementations of this interface transforms supplied model in some way.
 */
class IConfigTransformer
{
public:
    virtual BIO_XML_NS::model::Model* transform(BIO_XML_NS::model::Model* config) = 0;
};


BIO_CFG_NS_END
#endif
