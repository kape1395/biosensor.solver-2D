#include <iostream>
//#include <log4cxx/logger.h>
#include <log4cpp/Category.hh>
#include <log4cpp/PropertyConfigurator.hh>
#include <bio/Exception.hxx>
#include <bio/slv/ISolver.hxx>
#include <biosensor-slv-fd.hxx>
#include <xercesc/util/PlatformUtils.hpp>
#include <Model.hxx>

using namespace BIO_NS;
using namespace BIO_SLV_NS;
using namespace BIO_XML_NS::model;

#include <bio/cfg/StructureAnalyzer.hxx>
#include <bio/slv/ISolverListener.hxx>
#include <bio/slv/InvokeEverySL.hxx>
#include <bio/slv/StopAtStepSL.hxx>
#include <bio/io/DebugSL.hxx>

/**
 *  Entry point for program bio-solver.
 */
int main(int argn, char **argv)
{
    //LoggerPtr log(Logger::getLogger("bio-solver"));
    log4cpp::PropertyConfigurator::configure("log4cpp.properties");
    log4cpp::Category& log = log4cpp::Category::getInstance("bio-solver");


    if (argn == 1)
    {
        printf("usage: bio-solver <file-name>\n");
        printf("\tfile-name\tBiosensor configuration XML file\n");
        return 1;
    }
    else
    {
        log.debug("Starting");

        XERCES_CPP_NAMESPACE::XMLPlatformUtils::Initialize();
        try
        {

            // Parse file
            log.info("Parsing config file...");
            const std::string uri = std::string(argv[1]);
            std::auto_ptr<Model> model(BIO_XML_NS::model::model(uri));

            // Create solver
            log.info("Creating solver...");
            ISolver* solver = 0;
            {
                // FIXME: Only one solver factory exists for now, so we are using it explicitly
                BIO_SLV_FD_NS::SolverFactory solverFactory;

                if ((solver = solverFactory.create(&*model)) == 0)
                {
                    log.error("I dont know how to create requested solver.");
                    return 2;
                }
            }



            // TODO: Implement listeners in the configurable way.
            std::vector<ISolverListener*> listeners;
            ISolverListener* listener;
            InvokeEverySL* listenerInvokeEvery;

            listeners.push_back(listener = new BIO_SLV_NS::StopAtStepSL(solver, 2));
            dynamic_cast<IIterativeSolver*>(solver)->addListener(listener);

            listeners.push_back(listener = listenerInvokeEvery = new BIO_SLV_NS::InvokeEverySL(solver, 1000));
            dynamic_cast<IIterativeSolver*>(solver)->addListener(listener);

            listeners.push_back(listener = new BIO_IO_NS::DebugSL(solver, std::cout));
            listenerInvokeEvery->addListener(listener);



            // Solve biosensor
            log.info("Solving...");
            solver->solve();

            // Deinitialize
            for (std::vector<ISolverListener*>::iterator sl = listeners.begin(); sl < listeners.end(); sl++)
            {
                delete *sl;
            }
            listeners.empty();

            delete solver;
            log.info("Success");

            XERCES_CPP_NAMESPACE::XMLPlatformUtils::Terminate();
            log4cpp::Category::shutdown();
            return 0;

        }
        catch (const xml_schema::exception& e)
        {
            log.error(e.what());

            XERCES_CPP_NAMESPACE::XMLPlatformUtils::Terminate();
            log4cpp::Category::shutdown();
            return 2;
        }
        catch (Exception& ee)
        {
            log.error(ee.what());

            XERCES_CPP_NAMESPACE::XMLPlatformUtils::Terminate();
            log4cpp::Category::shutdown();
            return 2;
        }
    }
}
