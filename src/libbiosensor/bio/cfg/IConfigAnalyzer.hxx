#ifndef BIO_CFG_IConfigAnalyzer_HXX
#define BIO_CFG_IConfigAnalyzer_HXX
#include "../../biosensor.hxx"
#include <biosensor-xml.hxx>
BIO_CFG_NS_BEGIN


/**
 *  Implementations of this interface privide configuration information
 *  in some structured way.
 */
class IConfigAnalyzer
{
public:
    virtual void analyze(BIO_XML_NS::model::Model* config) = 0;
};


BIO_CFG_NS_END
#endif
