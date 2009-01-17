#ifndef BIO_IO_GnuplotDataSL_HXX
#define BIO_IO_GnuplotDataSL_HXX
#include "../../biosensor.hxx"
#include "../slv/ISolverListener.hxx"
#include <log4cxx/logger.h>
#include <ostream>
BIO_IO_NS_BEGIN


/**
 *
 */
class GnuplotDataSL : public BIO_SLV_NS::ISolverListener
{
private:
    log4cxx::LoggerPtr log;
    std::ostream& out;
    BIO_SLV_NS::ISolver* solver;

public:
    /**
     *  Constructor.
     */
    GnuplotDataSL(
        BIO_SLV_NS::ISolver* solver,
        std::ostream& output
    );

    /**
     *  Destructor.
     */
    virtual ~GnuplotDataSL();

    /**
     *  EventListener.
     */
    virtual void solveEventOccured();

};



BIO_IO_NS_END
#endif
