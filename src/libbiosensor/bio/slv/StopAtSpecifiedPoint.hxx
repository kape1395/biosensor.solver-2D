#ifndef BIO_SLV_StopAtSpecifiedPoint_HXX
#define BIO_SLV_StopAtSpecifiedPoint_HXX
#include "../../biosensor.hxx"
#include "ISolverListener.hxx"
#include <log4cxx/logger.h>
BIO_SLV_NS_BEGIN


/**
 *
 */
class StopAtSpecifiedPoint : public ISolverListener
{
private:
    log4cxx::LoggerPtr log;
    IIterativeSolver* solver;
    long stepCount;
    double time;

public:
    /**
     *  Constructor.
     */
    StopAtSpecifiedPoint(
        ISolver* solver
    );

    /**
     *
     */
    void setStepCount(long stepCount)
    {
        this->stepCount = stepCount;
    }

    /**
     *
     */
    void setTime(double time)
    {
        this->time = time;
    }

    /**
     *  Destructor.
     */
    virtual ~StopAtSpecifiedPoint();

    /**
     *  EventListener.
     */
    virtual void solveEventOccured();

};



BIO_SLV_NS_END
#endif
