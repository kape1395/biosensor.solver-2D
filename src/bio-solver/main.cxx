#include <iostream>
#include <vector>
#include <string>
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
 *  Planned parameters:
 *      -m --model <model.xml>
 *      -d --data <dataDir>
 *      -Sname=value
 *      --catalog <catalog.xml>     -- xerces-c does not support xml catalogs :(
 *      --no-schema-validation      
 *      ...
 *
 *
 */

class ModelSymbols
{
private:
    std::map<std::string, double> symbols;
public:
    ModelSymbols(int argn, char **argv);
    void override(BIO_XML_NS::model::Model& model);

};

/**
 *  Entry point for program bio-solver.
 */
int main(int argn, char **argv)
{
    using namespace BIO_NS;
    using namespace BIO_IO_NS;
    using namespace BIO_SLV_NS;
    using namespace BIO_XML_NS::model;

    if (argn < 3)
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
        std::stringstream commandLine;
        for (int i = 0; i < argn; i++)
        {
            commandLine << argv[i];
            if (i != argn - 1)
                commandLine << ' ';
        }

        LOG_DEBUG(LOGGER << "Starting...");

        XERCES_CPP_NAMESPACE::XMLPlatformUtils::Initialize();
        try
        {
            boost::filesystem::path configPath(argv[1]);
            boost::filesystem::path outputPath(argv[2]);

            LOG_INFO(LOGGER << "Command line is          : " << commandLine.str());
            LOG_INFO(LOGGER << "Using configuration file : " << configPath.file_string());
            LOG_INFO(LOGGER << "Using output directory   : " << outputPath.directory_string());

            ModelSymbols modelSymbols(argn, argv);

            // Parse file
            LOG_INFO(LOGGER << "Parsing config file...");
            std::auto_ptr<Model> model;
            {
                std::filebuf configFileBuf;
                configFileBuf.open(configPath.file_string().c_str(), std::ios::in);
                std::istream configIStream(&configFileBuf);

                long flags = 0;
                bool validateSchema = true;
                if (!validateSchema)
                    flags |= xml_schema::flags::dont_validate;
                
                model = BIO_XML_NS::model::model(configIStream, flags);
                
                configFileBuf.close();
            }
            LOG_INFO(LOGGER << "Parsing config file... Done");


            // Apply all modifications to the model.
            modelSymbols.override(*model);


            // Construct factories.
            IContext* context = new FilesystemContext(outputPath.directory_string());

            {   // save original model.
                std::filebuf configFileBuf;
                configFileBuf.open(configPath.file_string().c_str(), std::ios::in);
                std::istream configIStream(&configFileBuf);

                context->setOriginalConfiguration(configIStream);

                configFileBuf.close();
            }
            {   // save actual model.
                std::stringstream buf;
                std::stringstream schemaUri;
                std::string schemaUriPrefix;
                schemaUri << "http://karolis.5grupe.lt/biosensor/" << BIO_VERSION << "/schemas/";
                schemaUriPrefix = schemaUri.str();

                xml_schema::namespace_infomap map;

                schemaUri.clear();
                schemaUri << schemaUriPrefix << "Model.xsd";
                map[""].name    = "http://lt.5grupe.karolis/biosensor/model";
                map[""].schema  = schemaUri.str();

                schemaUri.clear();
                schemaUri << schemaUriPrefix << "ModelBound.xsd";
                map["b"].name   = "http://lt.5grupe.karolis/biosensor/model/bound";
                map["b"].schema = schemaUri.str();
                
                schemaUri.clear();
                schemaUri << schemaUriPrefix << "ModelReaction.xsd";
                map["r"].name   = "http://lt.5grupe.karolis/biosensor/model/reaction";
                map["r"].schema = schemaUri.str();

                schemaUri.clear();
                schemaUri << schemaUriPrefix << "ModelSolver.xsd";
                map["s"].name   = "http://lt.5grupe.karolis/biosensor/model/solver";
                map["s"].schema = schemaUri.str();

                schemaUri.clear();
                schemaUri << schemaUriPrefix << "ModelTransducer.xsd";
                map["t"].name   = "http://lt.5grupe.karolis/biosensor/model/transducer";
                map["t"].schema = schemaUri.str();

                BIO_XML_MODEL_NS::model(buf, *model, map);
                context->setActualConfiguration(buf);
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
            LOG_INFO(LOGGER << "#");
            LOG_INFO(LOGGER << "# SIMULATION SUCCESSFUL");
            LOG_INFO(LOGGER << "#");


            XERCES_CPP_NAMESPACE::XMLPlatformUtils::Terminate();
            return 0;

        }
        catch (const xml_schema::exception& e)
        {
            LOG_ERROR(LOGGER << "xml_schema::exception: " << e.what() << ". Error description is:\n" << e);
            LOG_INFO(LOGGER << "#");
            LOG_INFO(LOGGER << "# SIMULATION FAILED");
            LOG_INFO(LOGGER << "#");
            XERCES_CPP_NAMESPACE::XMLPlatformUtils::Terminate();
            return 2;
        }
        catch (Exception& ee)
        {
            LOG_ERROR(LOGGER << "bio::Exception: " << ee.what());
            LOG_INFO(LOGGER << "#");
            LOG_INFO(LOGGER << "# SIMULATION FAILED");
            LOG_INFO(LOGGER << "#");
            XERCES_CPP_NAMESPACE::XMLPlatformUtils::Terminate();
            return 2;
        }
        catch (...)
        {
            LOG_ERROR(LOGGER << "Unknown error");
            LOG_INFO(LOGGER << "#");
            LOG_INFO(LOGGER << "# SIMULATION FAILED");
            LOG_INFO(LOGGER << "#");
            XERCES_CPP_NAMESPACE::XMLPlatformUtils::Terminate();
            return 2;
        }
    }
}


/* ************************************************************************** */
ModelSymbols::ModelSymbols(int argn, char **argv)
{
    std::string key = "-S";

    for (int i = 3; i < argn; i++)
    {
        std::string arg(argv[i]);
        if (std::string(arg).compare(0, key.length(), key) != 0)
            continue;

        if (arg.find('=') == std::string::npos)
            throw bio::Exception("Symbol override parameters must be in form: -SsymbolName=symbolValue");

        std::string symbolName = arg.substr(key.length(), arg.find('=') - key.length());
        std::string symbolValueStr = arg.substr(arg.find('=') + 1);
        double symbolValue;

        std::stringstream tmp(symbolValueStr);
        tmp >> symbolValue;

        symbols.insert(symbols.end(), std::pair<std::string, double>(symbolName, symbolValue));

        LOG_DEBUG(LOGGER << "Found symbol override parameter \"" << arg
                  << "\", setting symbol \"" << symbolName
                  << "\" value to " << symbolValue
                 );
    }
}


/* ************************************************************************** */
void ModelSymbols::override(BIO_XML_NS::model::Model& model)
{
    for (std::map<std::string, double>::iterator so = symbols.begin(); so != symbols.end(); so++)
    {
        bool foundInModel = false;
        for (BIO_XML_NS::model::Model::symbol_sequence::iterator s = model.symbol().begin(); s < model.symbol().end(); s++)
        {
            if (s->name().compare(so->first) == 0)
            {
                foundInModel = true;
                s->value(so->second);
                break;
            }
        }
        if (!foundInModel)
            throw bio::Exception("Symbol, specified to be overriden was not found in the model");
    }
}


/* ************************************************************************** */
