#ifndef BIO_IO_DebugSL_HXX
#define BIO_IO_DebugSL_HXX
#include "../../biosensor.hxx"
#include "../slv/ISolverListener.hxx"
#include "../dm/IGrid2D.hxx"
#include "../dm/ICursor2D.hxx"
#include <log4cxx/logger.h>
#include <ostream>
BIO_IO_NS_BEGIN


/**
 *
 */
class DebugSL : public BIO_SLV_NS::ISolverListener
{
private:
    log4cxx::LoggerPtr log;
    std::ostream& out;
    BIO_SLV_NS::ISolver* solver;
    BIO_DM_NS::IGrid2D* grid;
    BIO_DM_NS::ICursor2D* cursor;

public:
    /**
     *  Constructor.
     */
    DebugSL(
        BIO_SLV_NS::ISolver* solver,
        std::ostream& output
    );

    /**
     *  Destructor.
     */
    virtual ~DebugSL();

    /**
     *  EventListener.
     */
    virtual void solveEventOccured();

};



BIO_IO_NS_END
#endif
