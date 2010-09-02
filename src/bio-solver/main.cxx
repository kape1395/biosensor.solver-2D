#include <iostream>
#include <vector>
#include <string>
#include <ctime>
#include <boost/filesystem/path.hpp>
#include <boost/filesystem.hpp>
#include <bio/Exception.hxx>
#include <bio/Logging.hxx>
#include <bio/MainFactory.hxx>
#include <bio/DelegatingFactory.hxx>
#include <bio/io/IContext.hxx>
#include <bio/io/FilesystemContext.hxx>
#include <bio/io/ConcentrationProfileReader.hxx>
#include <bio/slv/ISolver.hxx>
#include <bio/slv/IIterativeSolver.hxx>
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
 *  Prints help message for the program.
 */
void print_usage(const std::string& errMsg);


/**
 *  Entry point for program bio-solver.
 */
int main(int argn, char **argv)
{
    std::clock_t clock_start = std::clock();

    using namespace BIO_NS;
    using namespace BIO_IO_NS;
    using namespace BIO_SLV_NS;
    using namespace BIO_XML_NS::model;

    char* paramModelFile = 0;
    char* paramOutputDir = 0;
    bool resumeMode = false;
    char* concentrationFile = 0;

    if (argn < 2)
    {
        print_usage("Missing parameters.");
        return 1;
    }
    else if (argn >= 2 && !std::strcmp(argv[1], "--resume"))
    {
        //      0          1         2               3                4
        //  bio-solver --resume <file-name> <concetrtation-file> <output-dir>
        LOG_INFO(LOGGER << "Starting simulation in RESUME mode.");
        if (argn < 5)
        {
            print_usage("Missing parameters for RESUME mode.");
            return 1;
        }
        paramModelFile = argv[2];
        concentrationFile = argv[3];
        paramOutputDir = argv[4];
        resumeMode = true;
    }
    else if (argn >= 2 && !std::strcmp(argv[1], "--simulate"))
    {
        //      0            1            2          3
        //  bio-solver --simulate <file-name> <output-dir>
        LOG_INFO(LOGGER << "Starting simulation in SIMULATE mode.");
        if (argn < 4)
        {
            print_usage("Missing parameters for SIMULATE mode.");
            return 1;
        }
        paramModelFile = argv[2];
        paramOutputDir = argv[3];
        concentrationFile = 0;
        resumeMode = false;
    }
    else
    {
        //      0           1           2
        //  bio-solver <file-name> <output-dir>
        LOG_INFO(LOGGER << "Starting simulation in SIMULATE mode.");
        if (argn < 3)
        {
            print_usage("Missing parameters (non resume mode).");
            return 1;
        }
        paramModelFile = argv[1];
        paramOutputDir = argv[2];
        concentrationFile = 0;
        resumeMode = false;
    }

    //
    //////////////////////////////////////////
    //

    std::stringstream commandLine;
    for (int i = 0; i < argn; i++)
    {
        commandLine << argv[i];
        if (i != argn - 1)
            commandLine << ' ';
    }

    XERCES_CPP_NAMESPACE::XMLPlatformUtils::Initialize();
    int error = -1;
    try
    {
        boost::filesystem::path configPath(paramModelFile);
        boost::filesystem::path outputPath(paramOutputDir);

        LOG_INFO(LOGGER << "Command line is          : " << commandLine.str());
        LOG_INFO(LOGGER << "Using configuration file : " << configPath.file_string());
        LOG_INFO(LOGGER << "Using output directory   : " << outputPath.directory_string());

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
        if (!resumeMode)
        {
            ModelSymbols modelSymbols(argn, argv);
            modelSymbols.override(*model);
        }
        else
        {
            LOG_WARN(LOGGER << "Symbol overriding is skipped in the case of resume mode.");
        }


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

            //  TODO: Specify schema locations using parameters, or by default in FS (as is installed).
            schemaUri << "http://karolis.5grupe.lt/biosensor/" << BIO_VERSION << "/schemas/";
            schemaUriPrefix = schemaUri.str();

            xml_schema::namespace_infomap map;

            schemaUri.str(schemaUriPrefix);
            schemaUri.seekp(0, std::ios_base::end);
            schemaUri << "Model.xsd";
            map[""].name    = "http://karolis.5grupe.lt/biosensor/model";
            map[""].schema  = schemaUri.str();

            schemaUri.str(schemaUriPrefix);
            schemaUri.seekp(0, std::ios_base::end);
            schemaUri << "ModelBound.xsd";
            map["b"].name   = "http://karolis.5grupe.lt/biosensor/model/bound";
            map["b"].schema = schemaUri.str();

            schemaUri.str(schemaUriPrefix);
            schemaUri.seekp(0, std::ios_base::end);
            schemaUri << "ModelReaction.xsd";
            map["r"].name   = "http://karolis.5grupe.lt/biosensor/model/reaction";
            map["r"].schema = schemaUri.str();

            schemaUri.str(schemaUriPrefix);
            schemaUri.seekp(0, std::ios_base::end);
            schemaUri << "ModelSolver.xsd";
            map["s"].name   = "http://karolis.5grupe.lt/biosensor/model/solver";
            map["s"].schema = schemaUri.str();

            schemaUri.str(schemaUriPrefix);
            schemaUri.seekp(0, std::ios_base::end);
            schemaUri << "ModelTransducer.xsd";
            map["t"].name   = "http://karolis.5grupe.lt/biosensor/model/transducer";
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


        if (resumeMode)
        {
            boost::filesystem::path concentrationPath(concentrationFile);
            std::ifstream input;
            input.exceptions(std::ifstream::badbit);
            input.open(concentrationPath.file_string().c_str(), std::ios::in);
            
            ConcentrationProfileReader concentrationReader(
                &*model,
                input
            );
            solver->setState(&concentrationReader);
            
            input.close(); // TODO: Close on exception...
        }

        // Simulate operation of the biosensor
        LOG_INFO(LOGGER << "Solving...");
        solver->solve();
        LOG_INFO(LOGGER << "Solving... Done");


        // Was simulation successful?
        error = solver->isSteadyStateReached() ? 0 : 1;


        delete solver;
        delete factory;
        delete context;
    }
    catch (const xml_schema::exception& e)
    {
        LOG_ERROR(LOGGER << "xml_schema::exception: " << e.what() << ". Error description is:\n" << e);
        error = 2;
    }
    catch (Exception& ee)
    {
        LOG_ERROR(LOGGER << "bio::Exception: " << ee.what());
        error = 2;
    }
    catch (...)
    {
        LOG_ERROR(LOGGER << "Unknown error");
        error = 2;
    }


    std::clock_t clock_end = std::clock();
    std::clock_t duration = ((clock_end - clock_start) / CLOCKS_PER_SEC);

    LOG_INFO(LOGGER << "#");
    LOG_INFO(LOGGER << "# Simulation " << (!error ? "SUCCESSFUL" : "FAILED"));
    LOG_INFO(LOGGER << "#");
    LOG_INFO(LOGGER << "# Execution took " << duration << " seconds (~" << (duration / 60) << " minutes)");
    LOG_INFO(LOGGER << "# Exiting with errCode=" << error);
    LOG_INFO(LOGGER << "#");
    XERCES_CPP_NAMESPACE::XMLPlatformUtils::Terminate();
    return error;
}

/* ************************************************************************** */
void print_usage(const std::string& errMsg)
{
    if (errMsg.length() > 0)
    {
        std::cerr << "#\n# " << errMsg << "\n";
    }
    std::cerr
        << "#\n"
        << "# usage: bio-solver [[--simulate] <file-name> <output-dir>] | [--resume <file-name> <concetrtation-file> <output-dir>]\n"
        << "# \tfile-name\tBiosensor configuration XML file\n"
        << "# \tconcentration-file\tConcentrarions used to resume from (including header with solved time and iteration number)\n"
        << "# \toutput-dir\tOutput directory. Must not exist on invocation.\n"
        << "#\n"
        ;
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
