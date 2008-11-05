#include "FiniteDifferencesSolverAnalyzer.hxx"
#include <bio/Exception.hxx>


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer::analyze(
    BIO_XML_NS::model::Model* config
)
{

    LOG4CXX_INFO(log, "cleanup...");

    ////////////////////////////////////////////////////////////////////////////
    //  Release all old data.
    //
    //
    //  Release all old data.
    ////////////////////////////////////////////////////////////////////////////

    LOG4CXX_INFO(log, "cleanup... Done");


    //  If 0 is passed as config, only cleanup should be performed.
    this->config = config;
    if (config == 0)
        return;


    LOG4CXX_INFO(log, "analyze...");


    LOG4CXX_INFO(log, "analyze... Done");
}

/* ************************************************************************** */
/* ************************************************************************** */
