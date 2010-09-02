#ifndef BIO_SLV_AdjustTimeStepByFactor_HXX
#define BIO_SLV_AdjustTimeStepByFactor_HXX
#include "../../biosensor.hxx"
#include "ISolver.hxx"
#include "ISolverListener.hxx"
BIO_SLV_NS_BEGIN


/**
 *  Increases time step regularly, until maximal step is reached.
 */
class AdjustTimeStepByFactor : public ISolverListener
{
private:
    BIO_SLV_NS::ISolver* solver;
    BIO_SLV_NS::IIterativeSolver* iterativeSolver;  ///< The same solver.

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

    /**
     *  Set next attempt to change step in time at least after specified
     *  number of steps.
     */
    virtual void scheduleNextAdjustment(long stepCount);

protected:

    /**
     *  Calculates new time step.
     */
    virtual double getNewTimeStep();

    /**
     *  Upadtes time step in the solver.
     */
    virtual void changeTimeStep(double newTimeStep);

    /**
     *  Returns solver, governed by this object.
     */
    BIO_SLV_NS::ISolver* getSolver()
    {
        return solver;
    }
    
    /**
     *  Returns solver, governed by this object.
     */
    BIO_SLV_NS::IIterativeSolver* getIterativeSolver()
    {
        return iterativeSolver;
    }

    /**
     *  Returns factor, used for increasing time step.
     */
    double getStepIncreaseFactor()
    {
        return factor;
    }
    
};



BIO_SLV_NS_END
#endif
