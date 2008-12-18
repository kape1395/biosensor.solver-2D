#ifndef BIO_SLV_FD_FiniteDifferencesSolverAnalyzer_HXX
#define BIO_SLV_FD_FiniteDifferencesSolverAnalyzer_HXX
#include "../../biosensor-slv-fd.hxx"
#include <biosensor-xml.hxx>
#include <bio/cfg/IConfigAnalyzer.hxx>
#include <bio/cfg/StructureAnalyzer.hxx>
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

    BIO_CFG_NS::StructureAnalyzer structureAnalyzer;
    BIO_XML_NS::model::solver::Axis** axisPartsH;
    BIO_XML_NS::model::solver::Axis** axisPartsV;


public:

    /**
     *  Constructor.
     */
    FiniteDifferencesSolverAnalyzer() :
            log(log4cxx::Logger::getLogger("libbiosensor-slv-fd::FiniteDifferencesSolverAnalyzer"))
    {
        this->config = 0;
        this->axisPartsH = 0;
        this->axisPartsV = 0;
    }

    /**
     *  Destructor.
     */
    virtual ~FiniteDifferencesSolverAnalyzer()
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


    BIO_XML_NS::model::solver::Axis* getAxisPartH(int index)
    {
        return axisPartsH[index];
    }

    BIO_XML_NS::model::solver::Axis* getAxisPartV(int index)
    {
        return axisPartsV[index];
    }

};


BIO_SLV_FD_NS_END
#endif
