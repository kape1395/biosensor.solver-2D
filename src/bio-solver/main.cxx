#include <iostream>
#include <log4cxx/logger.h>
#include <bio/Exception.hxx>
#include <bio/slv/ISolver.hxx>
#include <biosensor-slv-fd.hxx>
#include <xercesc/util/PlatformUtils.hpp>
#include <Model.hxx>

using namespace BIO_NS;
using namespace BIO_SLV_NS;
using namespace BIO_XML_NS::model;
using namespace log4cxx;

#include <bio/cfg/StructureAnalyzer.hxx>

/**
 *  Entry point for program bio-solver.
 */
int main(int argn, char **argv)
{
    LoggerPtr log(Logger::getLogger("bio-solver"));

    if (argn == 1)
    {
        printf("usage: bio-solver <file-name>\n");
        printf("\tfile-name\tBiosensor configuration XML file\n");
        return 1;
    }
    else
    {
        LOG4CXX_INFO(log, "Starting");

        XERCES_CPP_NAMESPACE::XMLPlatformUtils::Initialize();
        try
        {

            // Parse file
            LOG4CXX_INFO(log, "Parsing config file...");
            const std::string uri = std::string(argv[1]);
            std::auto_ptr<Model> model(BIO_XML_NS::model::model(uri));

            // Create solver
            LOG4CXX_INFO(log, "Creating solver...");
            ISolver* solver = 0;
            {
                // FIXME: Only one solver factory exists for now, so we are using it explicitly
                BIO_SLV_FD_NS::SolverFactory solverFactory;

                if ((solver = solverFactory.create(&*model)) == 0)
                {
                    LOG4CXX_ERROR(log, "I dont know how to create requested solver.");
                    return 2;
                }
            }

            // Solve biosensor
            LOG4CXX_INFO(log, "Solving...");
            solver->solve();

            // Deinitialize
            delete solver;
            LOG4CXX_INFO(log, "Success");
            return 0;

        }
        catch (const xml_schema::exception& e)
        {
            LOG4CXX_ERROR(log, e);
            return 2;
        }
        catch (Exception& ee)
        {
            LOG4CXX_ERROR(log, ee.what());
            return 2;
        }
        XERCES_CPP_NAMESPACE::XMLPlatformUtils::Terminate();
    }
}
