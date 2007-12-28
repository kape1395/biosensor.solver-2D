#ifndef BIO_SLV_ABSTRACTSOLVER_HXX
#define BIO_SLV_ABSTRACTSOLVER_HXX
#include "../../biosensor.hxx"
#include "ISolver.hxx"
#include "../dm/IDataModel.hxx"
#include <biosensor-xml.hxx>
BIO_SLV_NS_BEGIN


/**
 *
 */
class AbstractSolver : public ISolver
{
protected:
    BIO_XML_NS::model::Model*   config;
    BIO_DM_NS::IDataModel*      data;
    
public:
    AbstractSolver(BIO_XML_NS::model::Model* config);
    virtual ~AbstractSolver();
    virtual BIO_DM_NS::IDataModel* getData();
    virtual BIO_XML_NS::model::Model* getConfig();

};



BIO_SLV_NS_END
#endif
