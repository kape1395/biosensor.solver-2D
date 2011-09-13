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
#ifndef BIO_SLV_IIterativeSolver_HXX
#define BIO_SLV_IIterativeSolver_HXX
#include "../../biosensor.hxx"
#include "ISolver.hxx"
BIO_SLV_NS_BEGIN
class ISolverListener;


/**
 *
 */
class IIterativeSolver //: public ISolver -- to avoid diamond inheritance
{
public:
    virtual ~IIterativeSolver()
    {
        //  Empty virtual destructor.
    }

    virtual void stop(bool steadyStateReached = false) = 0;
    virtual void resume() = 0;
    virtual bool isStopped() = 0;
    virtual double getTimeStep() = 0;
    virtual void setTimeStep(double timeStep) = 0;
    virtual long getSolvedIterationCount() = 0;
    virtual double getSolvedTime() = 0;
    virtual void addListener(ISolverListener* listener, bool deleteOnDestruction) = 0;

    /**
     *  Used to do resume for the simulation.
     */
    virtual void setIterationState(long solvedIterationCount, double solvedTime, double timeStep) = 0;
};



BIO_SLV_NS_END
#endif
