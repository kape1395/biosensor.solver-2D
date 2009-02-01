#ifndef BIO_IFactory_HXX
#define BIO_IFactory_HXX
#include "../biosensor.hxx"
#include "slv/ISolver.hxx"
#include "slv/ISolverListener.hxx"
#include "slv/ITransducer.hxx"
#include <biosensor-xml.hxx>

BIO_NS_BEGIN


/**
 */
class IFactory
{
public:

    virtual ~IFactory()
    {
        //  Empty virtual destructor.
    }

    /**
     *  Create a solver from a model.
     *
     *  \returns
     *      Created solver or 0 - if does not know the solver,
     *      specified in the model.
     */
    virtual BIO_SLV_NS::ISolver* createSolver(
        BIO_XML_MODEL_NS::Model* model
    ) = 0;

    virtual BIO_SLV_NS::ISolverListener* createStopCondition(
        BIO_SLV_NS::ISolver* solver,
        BIO_XML_MODEL_NS::solver::StopCondition* stopCondition
    ) = 0;

    virtual BIO_SLV_NS::ISolverListener* createOutput(
        BIO_SLV_NS::ISolver* solver,
        BIO_XML_MODEL_NS::SolverOutput* output
    ) = 0;

    virtual BIO_SLV_NS::ITransducer* createTransducer(
        BIO_SLV_NS::ISolver* solver,
        BIO_XML_MODEL_NS::Transducer* transducer
    ) = 0;

};

BIO_NS_END

#endif
