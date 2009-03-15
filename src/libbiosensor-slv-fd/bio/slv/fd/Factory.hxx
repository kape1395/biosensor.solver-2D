#ifndef BIO_SLV_FD_Factory_HXX
#define BIO_SLV_FD_Factory_HXX
#include "../../../biosensor-slv-fd.hxx"
#include <bio/IFactory.hxx>
BIO_SLV_FD_NS_BEGIN


/**
 *  Created solvers, that relates to this module (libbiosensor-slf-fd).
 */
class Factory : public IFactory
{
private:
    IFactory* rootFactory;

public:

    /**
     *
     */
    Factory(IFactory* rootFactory);

    /**
     *
     */
    virtual ~Factory();

    /**
     *  Create a solver from a model.
     *
     *  \returns
     *      Created solver or 0 - if does not know the solver,
     *      specified in the model.
     */
    virtual BIO_SLV_NS::ISolver* createSolver(
        BIO_XML_MODEL_NS::Model* model
    );

    /**
     *
     */
    virtual BIO_SLV_NS::ISolverListener* createStopCondition(
        BIO_SLV_NS::ISolver* solver,
        BIO_XML_MODEL_NS::solver::StopCondition* stopCondition
    );

    /**
     *  Create time step adjuster by specification.
     */
    virtual BIO_SLV_NS::ISolverListener* createTimeStepAdjuster(
        BIO_SLV_NS::ISolver* solver,
        BIO_XML_MODEL_NS::solver::TimeStepAdjuster* timeStepAdjuster
    );

    /**
     *
     */
    virtual BIO_SLV_NS::ISolverListener* createOutput(
        BIO_SLV_NS::ISolver* solver,
        BIO_XML_MODEL_NS::SolverOutput* output
    );

    /**
     *
     */
    virtual BIO_SLV_NS::ITransducer* createTransducer(
        BIO_SLV_NS::ISolver* solver,
        BIO_XML_MODEL_NS::Transducer* transducer
    );

};

BIO_SLV_FD_NS_END
#endif
