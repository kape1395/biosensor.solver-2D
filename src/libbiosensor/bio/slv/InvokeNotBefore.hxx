#ifndef BIO_SLV_InvokeNotBefore_HXX
#define BIO_SLV_InvokeNotBefore_HXX
#include "../../biosensor.hxx"
#include "ISolverListener.hxx"
#include <log4cxx/logger.h>
#include <vector>
BIO_SLV_NS_BEGIN


/**
 *
 */
class InvokeNotBefore : public ISolverListener
{
private:
    log4cxx::LoggerPtr log;
    IIterativeSolver* solver;
    long stepCount;
    double time;

    std::vector<BIO_SLV_NS::ISolverListener*> listeners;

public:
    /**
     *  Constructor.
     */
    InvokeNotBefore(
        ISolver* solver
    );

    /**
     *  Destructor.
     */
    virtual ~InvokeNotBefore();

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
     *
     */
    void addListener(BIO_SLV_NS::ISolverListener* listener)
    {
        listeners.push_back(listener);
    }

    /**
     *  EventListener.
     */
    virtual void solveEventOccured();

};



BIO_SLV_NS_END
#endif
