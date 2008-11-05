#ifndef BIO_SLV_ISolver_HXX
#define BIO_SLV_ISolver_HXX
#include "../../biosensor.hxx"
#include "../dm/IDataModel.hxx"
#include <biosensor-xml.hxx>
BIO_SLV_NS_BEGIN


/**
 *
 */
class ISolver
{
public:
    virtual void solve() = 0;
    virtual BIO_DM_NS::IDataModel* getData() = 0;
    virtual BIO_XML_NS::model::Model* getConfig() = 0;
};



BIO_SLV_NS_END
#endif
