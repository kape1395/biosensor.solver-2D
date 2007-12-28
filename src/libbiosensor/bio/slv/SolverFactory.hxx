#ifndef BIO_SLV_SolverFactory_HXX
#define BIO_SLV_SolverFactory_HXX
#include "../../biosensor.hxx"
#include "ISolver.hxx"
#include <biosensor-xml.hxx>

BIO_SLV_NS_BEGIN


/**
 *  
 */
class SolverFactory
{
public:
    ISolver* create(BIO_XML_NS::model::Model* model);

};



BIO_SLV_NS_END
        
#endif
