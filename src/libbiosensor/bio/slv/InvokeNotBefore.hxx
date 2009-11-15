#ifndef BIO_SLV_InvokeNotBefore_HXX
#define BIO_SLV_InvokeNotBefore_HXX
#include "../../biosensor.hxx"
#include "ISolverListener.hxx"
#include <vector>
BIO_SLV_NS_BEGIN


/**
 *
 */
class InvokeNotBefore : public ISolverListener
{
private:
    typedef std::vector<BIO_SLV_NS::ISolverListener*> SLVector;

    IIterativeSolver* solver;
    long stepCount;
    double time;

    SLVector listeners;
    SLVector listenersToDelete;

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
     *  Add sub-listener.
     *
     *  \param listener             Sublistener.
     *  \param deleteAtDestruction  Delete listener at destruction.
     */
    void addListener(
        BIO_SLV_NS::ISolverListener* listener,
        bool deleteAtDestruction
    );

    /**
     *  EventListener.
     */
    virtual void solveEventOccured();
    
    /**
     *  Reset listener's internal state.
     */
    virtual void reset();

};



BIO_SLV_NS_END
#endif
