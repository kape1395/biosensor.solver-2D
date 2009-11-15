#ifndef BIO_SLV_InvokeEveryTimeStep_HXX
#define BIO_SLV_InvokeEveryTimeStep_HXX
#include "../../biosensor.hxx"
#include "ISolverListener.hxx"
#include <vector>
BIO_SLV_NS_BEGIN


/**
 *
 */
class InvokeEveryTimeStep : public ISolverListener
{
private:
    typedef std::vector<BIO_SLV_NS::ISolverListener*> SLVector;

    IIterativeSolver* solver;

    long    stepByStep;
    double  stepByTime;

    long    nextStopByStep;
    double  nextStopByTime;

    SLVector listeners;
    SLVector listenersToDelete;

public:

    /**
     *  Constructor.
     */
    InvokeEveryTimeStep(
        ISolver* solver
    );

    /**
     *  Destructor.
     */
    virtual ~InvokeEveryTimeStep();

    /**
     *  EventListener.
     */
    virtual void solveEventOccured();

    /**
     *  Reset listener's internal state.
     */
    virtual void reset();
    
    /**
     *  Add nested listener.
     */
    virtual void addListener(ISolverListener* listener, bool deleteOnDestruction);

    /**
     *
     */
    void setStepByStepCount(long stepCount)
    {
        this->stepByStep = stepCount;
    }

    /**
     *
     */
    void setStepByTime(double timeStep)
    {
        this->stepByTime = timeStep;
    }

};



BIO_SLV_NS_END
#endif
