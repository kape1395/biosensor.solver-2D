#include "MainFactory.hxx"

/* ************************************************************************** */
BIO_NS::MainFactory::MainFactory()
{
    //  Nothing to do here.
}


/* ************************************************************************** */
BIO_NS::MainFactory::~MainFactory()
{
    //  Nothing to do here.
}


/* ************************************************************************** */
BIO_SLV_NS::ISolver* BIO_NS::MainFactory::createSolver(
    BIO_XML_MODEL_NS::Model* model
)
{
    //  TODO: Implement
    return 0;
}


/* ************************************************************************** */
BIO_SLV_NS::ISolverListener* BIO_NS::MainFactory::createStopCondition(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::solver::StopCondition* stopCondition
)
{
    //  TODO: Implement
    return 0;
}


/* ************************************************************************** */
BIO_SLV_NS::ISolverListener* BIO_NS::MainFactory::createOutput(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::SolverOutput* output
)
{
    //  TODO: Implement
    return 0;
}


/* ************************************************************************** */
BIO_SLV_NS::ITransducer* BIO_NS::MainFactory::createTransducer(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::Transducer* transducer
)
{
    //  TODO: Implement
    return 0;
}

/* ************************************************************************** */
