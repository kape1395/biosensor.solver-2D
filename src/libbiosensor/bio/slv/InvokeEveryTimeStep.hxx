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
