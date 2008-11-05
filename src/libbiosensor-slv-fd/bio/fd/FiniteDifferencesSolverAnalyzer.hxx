#ifndef BIO_SLV_FD_FiniteDifferencesSolverAnalyzer_HXX
#define BIO_SLV_FD_FiniteDifferencesSolverAnalyzer_HXX
#include "../../biosensor-slv-fd.hxx"
#include <bio/cfg/IConfigAnalyzer.hxx>
#include <vector>
#include <log4cxx/logger.h>

BIO_SLV_FD_NS_BEGIN


/**
 *
 */
class FiniteDifferencesSolverAnalyzer : public BIO_CFG_NS::IConfigAnalyzer
{
private:
    log4cxx::LoggerPtr log;

    BIO_XML_NS::model::Model* config;


public:

    /**
     *  Constructor.
     */
    FiniteDifferencesSolverAnalyzer() :
            log(log4cxx::Logger::getLogger("libbiosensor-slv-fd::FiniteDifferencesSolverAnalyzer"))
    {
        this->config = 0;
    }

    /**
     *  Destructor.
     */
    ~FiniteDifferencesSolverAnalyzer()
    {
        analyze(0);
    }

    /**
     *  Setter for configuration, to be analyzed.
     *  Should analysis be performed in this method?
     *
     *  \param config  Configuration to be analyzed. This also can be 0.
     */
    virtual void analyze(BIO_XML_NS::model::Model* config);


};


BIO_SLV_FD_NS_END
#endif
