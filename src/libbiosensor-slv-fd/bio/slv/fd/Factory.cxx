#include "Factory.hxx"
#include <bio/Logging.hxx>
#include <bio/Exception.hxx>
#include "ex2d/Solver.hxx"
#include "im2d/Solver.hxx"
#define LOGGER "libbiosensor-slv-fd: "

/* ************************************************************************** */
BIO_SLV_FD_NS::Factory::Factory(IFactory* rootFactory)
{
    this->rootFactory = rootFactory;
}


/* ************************************************************************** */
BIO_SLV_FD_NS::Factory::~Factory()
{
    //  Nothing to do here.
}


/* ************************************************************************** */
BIO_SLV_NS::ISolver* BIO_SLV_FD_NS::Factory::createSolver(
    BIO_XML_MODEL_NS::Model* model
)
{
    if (model == 0)
        throw BIO_NS::Exception("Model is NULL, no solver can be created.");


    if (dynamic_cast<BIO_XML_NS::model::solver::Explicit1D*>(&model->solver()) != 0)
    {
        LOG_INFO(LOGGER << "Selected solver: bio::xml::model::solver::Explicit1D -> ???");
        throw BIO_NS::Exception("Not implemented: bio::xml::model::solver::Explicit1D");
    }
    else if (dynamic_cast<BIO_XML_NS::model::solver::Implicit1D*>(&model->solver()) != 0)
    {
        LOG_INFO(LOGGER << "Selected solver: bio::xml::model::solver::Implicit1D -> ???");
        throw BIO_NS::Exception("Not implemented: bio::xml::model::solver::Implicit1D");
    }
    else if (dynamic_cast<BIO_XML_NS::model::solver::Explicit2D*>(&model->solver()) != 0)
    {
        LOG_INFO(LOGGER << "Selected solver: bio::xml::model::solver::Explicit2D -> BIO_SLV_FD_NS::ex2d::Solver");
        BIO_SLV_FD_NS::ex2d::Solver* solver = new BIO_SLV_FD_NS::ex2d::Solver(model);

        return solver;
    }
    else if (dynamic_cast<BIO_XML_NS::model::solver::Implicit2D*>(&model->solver()) != 0)
    {
        LOG_INFO(LOGGER << "Selected solver: bio::xml::model::solver::Implicit2D -> BIO_SLV_FD_NS::im2d::Solver");
        BIO_SLV_FD_NS::im2d::Solver* solver = new BIO_SLV_FD_NS::im2d::Solver(
            model,
            rootFactory
        );

        return solver;
    }
    else
    {
        LOG_DEBUG(LOGGER << "I dont know the requested solver, so returning 0.");
        return 0;
    }
}


/* ************************************************************************** */
BIO_SLV_NS::ISolverListener* BIO_SLV_FD_NS::Factory::createStopCondition(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::solver::StopCondition* stopCondition
)
{
    //  This factory supplies no new stop conditions.
    return 0;
}


/* ************************************************************************** */
BIO_SLV_NS::ISolverListener* BIO_SLV_FD_NS::Factory::createOutput(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::SolverOutput* output
)
{
    //  This factory supplies no new output generators.
    return 0;
}


/* ************************************************************************** */
BIO_SLV_NS::ITransducer* BIO_SLV_FD_NS::Factory::createTransducer(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::Transducer* transducer
)
{
    //  This factory supplies no new transducers.
    return 0;
}

/* ************************************************************************** */
