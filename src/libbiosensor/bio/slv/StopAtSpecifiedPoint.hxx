#ifndef BIO_SLV_StopAtSpecifiedPoint_HXX
#define BIO_SLV_StopAtSpecifiedPoint_HXX
#include "../../biosensor.hxx"
#include "ISolverListener.hxx"
BIO_SLV_NS_BEGIN


/**
 *
 */
class StopAtSpecifiedPoint : public ISolverListener
{
private:

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

    /**
     *  Reset listener's internal state.
     */
    virtual void reset()
    {
        //  Nothing to reset.
    }
    
};



BIO_SLV_NS_END
#endif
