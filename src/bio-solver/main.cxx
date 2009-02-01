
#include "bio/DelegatingFactory.hxx"

#include <iostream>
//#include <log4cxx/logger.h>
#include <log4cpp/Category.hh>
#include <log4cpp/PropertyConfigurator.hh>
#include <bio/Exception.hxx>
#include <bio/MainFactory.hxx>
#include <bio/DelegatingFactory.hxx>
#include <bio/slv/ISolver.hxx>
#include <biosensor-slv-fd.hxx>
#include <xercesc/util/PlatformUtils.hpp>
#include <Model.hxx>
#include <bio/cfg/StructureAnalyzer.hxx>
//#include <bio/slv/ISolverListener.hxx>
//#include <bio/io/DebugSL.hxx>

/**
 *  Entry point for program bio-solver.
 */
int main(int argn, char **argv)
{
    using namespace BIO_NS;
    using namespace BIO_SLV_NS;
    using namespace BIO_XML_NS::model;

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
            log.info("Parsing config file... Done");


            // Construct factories.
            DelegatingFactory* factory = new DelegatingFactory();
            factory->addFactory(new BIO_NS::MainFactory(factory), true);
            factory->addFactory(new BIO_SLV_FD_NS::Factory(factory), true);

            
            // Create solver
            log.info("Creating solver...");
            ISolver* solver;
            if ((solver = factory->createSolver(&*model)) == 0)
            {
                log.error("I dont know how to create requested solver.");
                return 2;
            }
            log.info("Creating solver... Done");
            

            // Simulate operation of the biosensor
            log.info("Solving...");
            solver->solve();
            log.info("Solving... Done");


            delete solver;
            delete factory;
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
