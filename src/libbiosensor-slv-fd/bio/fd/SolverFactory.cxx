#include "SolverFactory.hxx"
#include <log4cxx/logger.h>
#include <bio/Exception.cxx>
#include "ex2d/Solver.hxx"


/**
 *  This method is responsible to create solvers, provided by this module.
 */
BIO_SLV_NS::ISolver* BIO_SLV_FD_NS::SolverFactory::create(BIO_XML_NS::model::Model* model)
{
    log4cxx::LoggerPtr log(log4cxx::Logger::getLogger("libbiosensor-slv-fd"));
    
    if (model == 0)
        throw bio::Exception("Model is NULL, no solver can be created.");
    
    
    if (dynamic_cast<bio::xml::model::s::Explicit1D*>(&model->solver()) != 0)
    {
        LOG4CXX_INFO(log, "Selected solver: bio::xml::model::s::Explicit1D -> ???");
        throw bio::Exception("Not implemented: bio::xml::model::s::Explicit1D");
    }
    else if (dynamic_cast<bio::xml::model::s::Explicit2D*>(&model->solver()) != 0)
    {
        LOG4CXX_INFO(log, "Selected solver: bio::xml::model::s::Explicit2D -> BIO_SLV_FD_NS::ex2d::Solver");
        BIO_SLV_FD_NS::ex2d::Solver* solver = new BIO_SLV_FD_NS::ex2d::Solver(model);
        
        return solver;
    }
    else
    {
        LOG4CXX_DEBUG(log, "I dont know the requested solver, so returning 0.");
        return 0;
    }
}
