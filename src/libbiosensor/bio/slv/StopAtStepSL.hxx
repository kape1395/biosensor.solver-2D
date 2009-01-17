#ifndef BIO_SLV_StopAtStepSL_HXX
#define BIO_SLV_StopAtStepSL_HXX
#include "../../biosensor.hxx"
#include "ISolverListener.hxx"
#include <log4cxx/logger.h>
BIO_SLV_NS_BEGIN


/**
 *
 */
class StopAtStepSL : public ISolverListener
{
private:
    log4cxx::LoggerPtr log;
    IIterativeSolver* solver;
    long stepNumber;

public:
    /**
     *  Constructor.
     */
    StopAtStepSL(
        ISolver* solver,
        long stepNumber
    );

    /**
     *  Destructor.
     */
    virtual ~StopAtStepSL();

    /**
     *  EventListener.
     */
    virtual void solveEventOccured();

};



BIO_SLV_NS_END
#endif
