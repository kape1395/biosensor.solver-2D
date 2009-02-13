#include <iostream>
#include <boost/filesystem/path.hpp>
#include <boost/filesystem.hpp>
#include <bio/Exception.hxx>
#include <bio/Logging.hxx>
#include <bio/MainFactory.hxx>
#include <bio/DelegatingFactory.hxx>
#include <bio/io/IContext.hxx>
#include <bio/io/FilesystemContext.hxx>
#include <bio/slv/ISolver.hxx>
#include <biosensor-slv-fd.hxx>
#include <xercesc/util/PlatformUtils.hpp>
#include <Model.hxx>
#include <bio/cfg/StructureAnalyzer.hxx>

#define LOGGER "bio-solver: "

/**
 *  Entry point for program bio-solver.
 */
int main(int argn, char **argv)
{
    using namespace BIO_NS;
    using namespace BIO_IO_NS;
    using namespace BIO_SLV_NS;
    using namespace BIO_XML_NS::model;

    if (argn != 3)
    {
        std::cerr
        << "usage: bio-solver <file-name> <output-dir>\n"
        << "\tfile-name\tBiosensor configuration XML file\n"
        << "\toutput-dir\tOutput directory. Must not exist on invocation.\n"
        ;
        return 1;
    }
    else
    {
        LOG_DEBUG(LOGGER << "Starting...");

        XERCES_CPP_NAMESPACE::XMLPlatformUtils::Initialize();
        try
        {
            boost::filesystem::path configPath(argv[1]);

            // Parse file
            LOG_INFO(LOGGER << "Parsing config file...");
            const std::string uri = std::string(argv[1]);
            std::auto_ptr<Model> model(BIO_XML_NS::model::model(uri));
            LOG_INFO(LOGGER << "Parsing config file... Done");


            // Construct factories.
            IContext* context = new FilesystemContext(std::string(argv[2]));
            {
                std::filebuf configFileBuf;
                configFileBuf.open(configPath.file_string().c_str(), std::ios::in);
                std::istream configIStream(&configFileBuf);
                context->setConfiguration(configIStream);
                configFileBuf.close();
            }

            DelegatingFactory* factory = new DelegatingFactory();
            factory->addFactory(new BIO_NS::MainFactory(factory, context), true);
            factory->addFactory(new BIO_SLV_FD_NS::Factory(factory), true);


            // Create solver
            LOG_INFO(LOGGER << "Creating solver...");
            ISolver* solver;
            if ((solver = factory->createSolver(&*model)) == 0)
            {
                LOG_ERROR(LOGGER << "I dont know how to create requested solver.");
                return 2;
            }
            LOG_INFO(LOGGER << "Creating solver... Done");


            // Simulate operation of the biosensor
            LOG_INFO(LOGGER << "Solving...");
            solver->solve();
            LOG_INFO(LOGGER << "Solving... Done");


            delete solver;
            delete factory;
            delete context;
            LOG_INFO(LOGGER << "Success");


            XERCES_CPP_NAMESPACE::XMLPlatformUtils::Terminate();
            return 0;

        }
        catch (const xml_schema::exception& e)
        {
            LOG_ERROR(LOGGER << "Exception: " << e.what());
            XERCES_CPP_NAMESPACE::XMLPlatformUtils::Terminate();
            return 2;
        }
        catch (Exception& ee)
        {
            LOG_ERROR(LOGGER << "Exception: " << ee.what());
            XERCES_CPP_NAMESPACE::XMLPlatformUtils::Terminate();
            return 2;
        }
    }
}
