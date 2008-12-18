#ifndef BIO_SLV_FD_FiniteDifferencesSolverAnalyzer_HXX
#define BIO_SLV_FD_FiniteDifferencesSolverAnalyzer_HXX
#include "../../biosensor-slv-fd.hxx"
#include <biosensor-xml.hxx>
#include <bio/cfg/StructureAnalyzer.hxx>
#include <vector>
#include <log4cxx/logger.h>

BIO_SLV_FD_NS_BEGIN


/**
 *
 */
class FiniteDifferencesSolverAnalyzer
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
     *  \param config  Configuration to be analyzed.
     */
    FiniteDifferencesSolverAnalyzer(BIO_XML_NS::model::Model* config);

    /**
     *  Destructor.
     */
    virtual ~FiniteDifferencesSolverAnalyzer();


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
