#ifndef BIO_SLV_ABSTRACTSOLVER_HXX
#define BIO_SLV_ABSTRACTSOLVER_HXX
#include "../../biosensor.hxx"
#include "ISolver.hxx"
#include "../dm/IDataModel.hxx"
#include <biosensor-xml.hxx>
BIO_SLV_NS_BEGIN


/**
 *  Abstract implementation of ISolver. It is very simple now.
 */
class AbstractSolver : public ISolver
{
private:
    BIO_XML_NS::model::Model* config;
    
public:
    
    /**
     *  Constructor.
     */
    AbstractSolver(BIO_XML_NS::model::Model* config)
    {
        this->config = config;
    }
    
    /**
     *  Destructor.
     */
    virtual ~AbstractSolver()
    {
        // Nothing to do here.
    }
    
    /**
     *  Returns configuration, supplied for this solver.
     */
    virtual BIO_XML_NS::model::Model* getConfig()
    {
        return this->config;
    }
    
};


BIO_SLV_NS_END
#endif
