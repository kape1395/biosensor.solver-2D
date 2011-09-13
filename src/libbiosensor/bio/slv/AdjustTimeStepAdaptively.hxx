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
#ifndef BIO_SLV_AdjustTimeStepAdaptively_HXX
#define BIO_SLV_AdjustTimeStepAdaptively_HXX
#include "../../biosensor.hxx"
#include "ISolverListener.hxx"
#include "ISolverStateHolder.hxx"
#include "AdjustTimeStepByFactor.hxx"
#include "ISolverListener.hxx"
#include "../io/ConcentrationProfile.hxx"
#include <vector>
BIO_SLV_NS_BEGIN


/**
 *  Adaptive time step adjuster.
 */
class AdjustTimeStepAdaptively : public ISolverListener
{
private:
    BIO_SLV_NS::ISolver* solver;
    BIO_SLV_NS::IIterativeSolver* iterativeSolver;  ///< The same solver.

    double increaseFactor;
    long increaseEveryStepCount;
    double increaseMaxTimeStep;

    double fallbackFactor;
    long fallbackForStepCount;
    long fallbackCheckEveryStepCount;
    double fallbackMinTimeStep;

    BIO_SLV_NS::ISolverStateHolder* solverStateHolder;
    std::vector<BIO_SLV_NS::ISolverListener*> stopConditions;

    long nextIterationForFallbackCheck;
    long nextIterationForIncrease;
    bool stateSaved;
    double failTime;

public:

    /**
     *  Constructor.
     */
    AdjustTimeStepAdaptively(
        ISolver* solver,
        double  increaseFactor,
        long    increaseEveryStepCount,
        double  increaseMaxTimeStep,
        double  fallbackFactor,
        long    fallbackForStepCount,
        long    fallbackCheckEveryStepCount,
        double  fallbackMinTimeStep,
        BIO_SLV_NS::ISolverStateHolder* solverStateHolder,
        std::vector<BIO_SLV_NS::ISolverListener*>& stopConditions
    );

    /**
     *  Destructor.
     */
    virtual ~AdjustTimeStepAdaptively();

    /**
     *  EventListener.
     */
    virtual void solveEventOccured();

    /**
     *  Reset listener's internal state.
     */
    virtual void reset();

protected:
    /**
     *  Just to be able to call from the constructor.
     */
    void resetInternal();

    /**
     *  Check if fallback is needed.
     */
    bool isFallbackNeeded();

    /**
     *  Do fallback in time.
     */
    void performFallback();

    /**
     *  Save current state.
     */
    void saveCurrentState();

    /**
     *  Was state saved?
     */
    bool isStateSaved();

    /**
     *  Increase time step;
     */
    void increaseTimeStep();

    /**
     *
     */
    void scheduleNextIncreaseAfter(long stepCount);

    /**
     *
     */
    void scheduleNextFallbackCheckAfter(long stepCount);

    bool isTimeForFallbackCheck();

    bool isTimeForIncrease();

};


BIO_SLV_NS_END
#endif
