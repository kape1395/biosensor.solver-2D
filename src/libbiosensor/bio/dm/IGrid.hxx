#ifndef BIO_DM_IGrid_HXX
#define BIO_DM_IGrid_HXX
#include "../../biosensor.hxx"
#include "IDataModel.hxx"
#include <biosensor-xml.hxx>

BIO_DM_NS_BEGIN


/**
 *  Base interface for all grid-based data models, such as finite differences
 *  data model. This interface is intented to be implemented by IDataModel
 *  implementations.
 */
class IGrid
{
public:
    virtual int getSubstanceCount() = 0;
    virtual BIO_XML_NS::model::Substance* getSubstanceConf(int index) = 0;
};



BIO_DM_NS_END

#endif
