#ifndef BIO_SLV_StopByCurrentDensityGradient_HXX
#define BIO_SLV_StopByCurrentDensityGradient_HXX
#include "../../biosensor.hxx"
#include "ISolverListener.hxx"
BIO_SLV_NS_BEGIN


/**
 *
 */
class StopByCurrentDensityGradient : public ISolverListener
{
private:
    class NextStepListener;

    BIO_SLV_NS::IIterativeSolver* iterativeSolver;
    BIO_SLV_NS::ITransducer* transducer;

    double gradient;
    bool normalized;
    long checkEveryNumberOfSteps;

    long nextStepForCheck;
    double prevCurrentDensity;
    double prevTime;

    NextStepListener* nextStepListener;

public:
    /**
     *  Constructor.
     */
    StopByCurrentDensityGradient(
        ISolver* solver,
        double gradient,
        bool normalized,
        long checkEveryNumberOfSteps = 100
    );

    /**
     *  Destructor.
     */
    virtual ~StopByCurrentDensityGradient();

    /**
     *  EventListener.
     */
    virtual void solveEventOccured();

private:

    /**
     *  Receives event, that the next step occured.
     */
    void processNextStep();

    /**
     *
     */
class NextStepListener : public ISolverListener
    {
    private:
        long processAfterIteration;
        bool actionRequested;
        StopByCurrentDensityGradient* stopCondition;

    public:
        NextStepListener(StopByCurrentDensityGradient* stopCondition)
        {
            this->stopCondition = stopCondition;
            this->processAfterIteration = 0;
            this->actionRequested = false;
        }

        virtual ~NextStepListener()
        {
            //  Nothing to do here.
        }

        virtual void solveEventOccured()
        {
            if (actionRequested && (stopCondition->iterativeSolver->getSolvedIterationCount() > processAfterIteration))
            {
                stopCondition->processNextStep();
                actionRequested = false;
            }
        }

        void listenForNextStep(long thisIteration)
        {
            processAfterIteration = thisIteration;
            actionRequested = true;
        }
    };

};



BIO_SLV_NS_END
#endif
