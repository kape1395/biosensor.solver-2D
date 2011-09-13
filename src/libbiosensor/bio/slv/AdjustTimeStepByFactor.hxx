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
