#ifndef BIO_IO_CurrentDensity_HXX
#define BIO_IO_CurrentDensity_HXX
#include "../../biosensor.hxx"
#include "../slv/ISolver.hxx"
#include "IOutput.hxx"
#include "IOutputContext.hxx"
#include <log4cxx/logger.h>
#include <string>
#include <ostream>
BIO_IO_NS_BEGIN


/**
 *
 */
class CurrentDensity : public BIO_IO_NS::IOutput
{
private:
    log4cxx::LoggerPtr log;

    std::string name;
    BIO_SLV_NS::ISolver* solver;
    BIO_IO_NS::IOutputContext* outputContext;

    std::ostream* output;

public:
    /**
     *  Constructor.
     */
    CurrentDensity(
        std::string& name,
        BIO_SLV_NS::ISolver* solver,
        BIO_IO_NS::IOutputContext* outputContext
    );

    /**
     *  Destructor.
     */
    virtual ~CurrentDensity();

    /**
     *  EventListener.
     */
    virtual void solveEventOccured();

};



BIO_IO_NS_END
#endif
