#ifndef BIO_IFactory_HXX
#define BIO_IFactory_HXX
#include "../biosensor.hxx"
#include "slv/ISolver.hxx"
#include "slv/ISolverListener.hxx"
#include "slv/ITransducer.hxx"
#include <biosensor-xml.hxx>

BIO_NS_BEGIN


/**
 *  Factory interface, that is implemented by all something-suplying modules.
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

    /**
     *  Create stop condition by specification.
     */
    virtual BIO_SLV_NS::ISolverListener* createStopCondition(
        BIO_SLV_NS::ISolver* solver,
        BIO_XML_MODEL_NS::solver::StopCondition* stopCondition
    ) = 0;

    /**
     *  Create time step adjuster by specification.
     */
    virtual BIO_SLV_NS::ISolverListener* createTimeStepAdjuster(
        BIO_SLV_NS::ISolver* solver,
        BIO_XML_MODEL_NS::solver::TimeStepAdjuster* timeStepAdjuster
    ) = 0;

    /**
     *  Create output generator by specification.
     */
    virtual BIO_SLV_NS::ISolverListener* createOutput(
        BIO_SLV_NS::ISolver* solver,
        BIO_XML_MODEL_NS::SolverOutput* output
    ) = 0;

    /**
     *  Create transducer by specification.
     */
    virtual BIO_SLV_NS::ITransducer* createTransducer(
        BIO_SLV_NS::ISolver* solver,
        BIO_XML_MODEL_NS::Transducer* transducer
    ) = 0;

};

BIO_NS_END

#endif
