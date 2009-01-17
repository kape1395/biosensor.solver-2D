#ifndef BIO_CFG_IConfigValidator_HXX
#define BIO_CFG_IConfigValidator_HXX
#include "../../biosensor.hxx"
#include <biosensor-xml.hxx>
#include <vector>
#include <string>
BIO_CFG_NS_BEGIN


/**
 *  Implementations of this interface validates model by some criterias.
 */
class IConfigValidator
{
public:
    virtual ~IConfigValidator()
    {
        //  Empty virtual destructor.
    }

    virtual bool validate(BIO_XML_NS::model::Model* config) = 0;
    virtual std::vector<std::string>& getFailReasons() = 0;
};


BIO_CFG_NS_END
#endif
