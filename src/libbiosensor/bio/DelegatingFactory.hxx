#ifndef BIO_DelegatingFactory_HXX
#define BIO_DelegatingFactory_HXX
#include "../biosensor.hxx"
#include "IFactory.hxx"
#include "slv/ISolver.hxx"
#include "slv/ISolverListener.hxx"
#include <biosensor-xml.hxx>
#include <vector>

BIO_NS_BEGIN


/**
 */
class DelegatingFactory : public IFactory
{
protected:
    std::vector<IFactory*> factories;


public:

    /**
     *
     */
    DelegatingFactory();

    /**
     *
     */
    virtual ~DelegatingFactory();

    /**
     *
     */
    void addFactory(IFactory* factory);

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

BIO_NS_END

#endif
