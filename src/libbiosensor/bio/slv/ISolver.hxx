#ifndef BIO_SLV_ISolver_HXX
#define BIO_SLV_ISolver_HXX
#include "../../biosensor.hxx"
#include "../dm/IDataModel.hxx"
#include "ITransducer.hxx"
#include <biosensor-xml.hxx>
BIO_SLV_NS_BEGIN


/**
 *  Solver performs a simulation of the specified biosensor.
 */
class ISolver
{
public:
    virtual ~ISolver()
    {
        //  Empty virtual destructor.
    }

    /**
     *  Simulate an elxecution of the specified biosensor. Biosensor model
     *  is specified during the construction of the solver..
     */
    virtual void solve() = 0;

    /**
     *  Returns current concentrations of the substances.
     */
    virtual BIO_DM_NS::IDataModel* getData() = 0;

    /**
     *  Returns configuration (model), that was used for the simulation.
     */
    virtual BIO_XML_NS::model::Model* getConfig() = 0;

    /**
     *  Returns transducer, used in the simulation. Transtuder generates an
     *  output of the biosensor.
     */
    virtual ITransducer* getTransducer() = 0;

};



BIO_SLV_NS_END
#endif
