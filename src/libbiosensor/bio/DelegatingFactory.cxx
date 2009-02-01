#include "DelegatingFactory.hxx"

/* ************************************************************************** */
BIO_NS::DelegatingFactory::DelegatingFactory()
{
    //  Nothing to do here.
}


/* ************************************************************************** */
BIO_NS::DelegatingFactory::~DelegatingFactory()
{
    factories.clear();
    for (std::vector<IFactory*>::iterator ftd = factoriesToDelete.begin();
            ftd < factoriesToDelete.end(); ftd++)
    {
        delete *ftd;
    }
    factoriesToDelete.clear();
}


/* ************************************************************************** */
void BIO_NS::DelegatingFactory::addFactory(IFactory* factory, bool deleteAtDestruction)
{
    factories.push_back(factory);
    if (deleteAtDestruction)
        factoriesToDelete.push_back(factory);
}


/* ************************************************************************** */
BIO_SLV_NS::ISolver* BIO_NS::DelegatingFactory::createSolver(
    BIO_XML_MODEL_NS::Model* model
)
{
    for (std::vector<IFactory*>::iterator f = factories.begin(); f < factories.end(); f++)
    {
        BIO_SLV_NS::ISolver* solver = (*f)->createSolver(model);
        if (solver)
            return solver;
    }
    return 0;
}


/* ************************************************************************** */
BIO_SLV_NS::ISolverListener* BIO_NS::DelegatingFactory::createStopCondition(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::solver::StopCondition* stopCondition
)
{
    for (std::vector<IFactory*>::iterator f = factories.begin(); f < factories.end(); f++)
    {
        BIO_SLV_NS::ISolverListener* sl = (*f)->createStopCondition(solver, stopCondition);
        if (sl)
            return sl;
    }
    return 0;
}


/* ************************************************************************** */
BIO_SLV_NS::ISolverListener* BIO_NS::DelegatingFactory::createOutput(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::SolverOutput* output
)
{
    for (std::vector<IFactory*>::iterator f = factories.begin(); f < factories.end(); f++)
    {
        BIO_SLV_NS::ISolverListener* sl = (*f)->createOutput(solver, output);
        if (sl)
            return sl;
    }
    return 0;
}


/* ************************************************************************** */
BIO_SLV_NS::ITransducer* BIO_NS::DelegatingFactory::createTransducer(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::Transducer* transducer
)
{
    for (std::vector<IFactory*>::iterator f = factories.begin(); f < factories.end(); f++)
    {
        BIO_SLV_NS::ITransducer* td = (*f)->createTransducer(solver, transducer);
        if (td)
            return td;
    }
    return 0;
}

/* ************************************************************************** */
