#ifndef BIO_SLV_AdjustTimeStepByFactor_HXX
#define BIO_SLV_AdjustTimeStepByFactor_HXX
#include "../../biosensor.hxx"
#include "ISolverListener.hxx"
BIO_SLV_NS_BEGIN


/**
 *
 */
class AdjustTimeStepByFactor : public ISolverListener
{
private:

    BIO_SLV_NS::IIterativeSolver* iterativeSolver;

    double  factor;
    long    adjustEveryNumberOfSteps;
    double  maxTimeStep;        //  Do not apply if 0.

    long nextStepForAdjustment;

public:

    /**
     *  Constructor.
     */
    AdjustTimeStepByFactor(
        ISolver* solver,
        double factor,
        long adjustEveryNumberOfSteps,
        double maxTimeStep = 0.0
    );

    /**
     *  Destructor.
     */
    virtual ~AdjustTimeStepByFactor();

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
