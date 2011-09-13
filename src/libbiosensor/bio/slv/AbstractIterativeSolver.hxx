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
#ifndef BIO_SLV_ABSTRACTITERATIVESOLVER_HXX
#define BIO_SLV_ABSTRACTITERATIVESOLVER_HXX
#include "../../biosensor.hxx"
#include "AbstractSolver.hxx"
#include "IIterativeSolver.hxx"
#include <vector>
BIO_SLV_NS_BEGIN
class ISolverListener;


/**
 *  Abstract operations: solveIteration().
 */
class AbstractIterativeSolver : public AbstractSolver, public IIterativeSolver
{
private:
    double timeStep;
    double timeSolved;
    long   iterationsSolved;
    bool   stopped;
    bool   steadyStateReached;
    std::vector<ISolverListener*> listeners;
    std::vector<ISolverListener*> listenersToDelete;

public:
    AbstractIterativeSolver(BIO_XML_NS::model::Model* config);
    virtual ~AbstractIterativeSolver();

    virtual void solve();
    virtual void stop(bool steadyStateReached = false);
    virtual void resume();
    virtual bool isStopped();
    virtual bool isSteadyStateReached();
    virtual double getTimeStep();
    virtual void setTimeStep(double timeStep);
    virtual long getSolvedIterationCount();
    virtual double getSolvedTime();
    virtual void addListener(ISolverListener* listener, bool deleteOnDestruction);
    /**
     *  Used to do resume for the simulation.
     */
    virtual void setIterationState(long solvedIterationCount, double solvedTime, double timeStep);

protected:
    virtual void invokeListeners();
    virtual void resetListeners();
    virtual void solveIteration() = 0;

};



BIO_SLV_NS_END

#endif
