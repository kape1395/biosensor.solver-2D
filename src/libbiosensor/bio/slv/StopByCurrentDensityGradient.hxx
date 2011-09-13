/*
 * Copyright 2011 Karolis Petrauskas
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
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

    /**
     *  Number of iterations already passed the test.
     */
    int passedIterations;

    static int MIN_PASSED_ITERATIONS;

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

    /**
     *  Reset listener's internal state.
     */
    virtual void reset();

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

        virtual void reset()
        {
            actionRequested = false;
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
