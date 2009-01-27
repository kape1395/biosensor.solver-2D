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
#include <bio/slv/ISolverListener.hxx>
#include <bio/slv/InvokeEverySL.hxx>
#include <bio/slv/StopAtStepSL.hxx>
#include <bio/io/DebugSL.hxx>

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



            // TODO: Implement listeners in the configurable way.
            std::vector<ISolverListener*> listeners;
            ISolverListener* listener;
            InvokeEverySL* listenerInvokeEvery;

            listeners.push_back(listener = new BIO_SLV_NS::StopAtStepSL(solver, 2000));
            dynamic_cast<IIterativeSolver*>(solver)->addListener(listener);

            listeners.push_back(listener = listenerInvokeEvery = new BIO_SLV_NS::InvokeEverySL(solver, 1000));
            dynamic_cast<IIterativeSolver*>(solver)->addListener(listener);

            listeners.push_back(listener = new BIO_IO_NS::DebugSL(solver, std::cout));
            listenerInvokeEvery->addListener(listener);



            // Solve biosensor
            LOG4CXX_INFO(log, "Solving...");
            solver->solve();

            // Deinitialize
            for (std::vector<ISolverListener*>::iterator sl = listeners.begin(); sl < listeners.end(); sl++)
            {
                delete *sl;
            }
            listeners.empty();

            delete solver;
            LOG4CXX_INFO(log, "Success");

            XERCES_CPP_NAMESPACE::XMLPlatformUtils::Terminate();
            return 0;

        }
        catch (const xml_schema::exception& e)
        {
            LOG4CXX_ERROR(log, e);

            XERCES_CPP_NAMESPACE::XMLPlatformUtils::Terminate();
            return 2;
        }
        catch (Exception& ee)
        {
            LOG4CXX_ERROR(log, ee.what());

            XERCES_CPP_NAMESPACE::XMLPlatformUtils::Terminate();
            return 2;
        }
    }
}
