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
#include "InvokeEveryTimeStep.hxx"
#include "../Exception.hxx"


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::InvokeEveryTimeStep::InvokeEveryTimeStep(
    ISolver* solver
)
{
    this->solver = dynamic_cast<IIterativeSolver*>(solver);
    this->stepByStep = 0l;
    this->stepByTime = 0.0;
    reset();

    if (this->solver == 0)
        throw Exception("InvokeEveryTimeStep: Solver must implement IIterativeSolver");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::InvokeEveryTimeStep::~InvokeEveryTimeStep()
{
    for (SLVector::iterator l = listenersToDelete.begin(); l < listenersToDelete.end(); l++)
    {
        delete *l;
    }
    listeners.clear();
    listenersToDelete.clear();
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::InvokeEveryTimeStep::solveEventOccured()
{
    if (    (stepByStep != 0l  && solver->getSolvedIterationCount() >= nextStopByStep) ||
            (stepByTime != 0.0 && solver->getSolvedTime()           >= nextStopByTime)
       )
    {
        for (unsigned i = 0; i < listeners.size(); i++)
        {
            listeners[i]->solveEventOccured();
        }
        nextStopByStep = solver->getSolvedIterationCount() + stepByStep;
        nextStopByTime = solver->getSolvedTime() + stepByTime;
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::InvokeEveryTimeStep::reset()
{
    // stop at the next step.
    nextStopByStep = solver->getSolvedIterationCount();
    nextStopByTime = solver->getSolvedTime();
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::InvokeEveryTimeStep::addListener(
    ISolverListener* listener,
    bool deleteOnDestruction
)
{
    listeners.push_back(listener);
    if (deleteOnDestruction)
        listenersToDelete.push_back(listener);
}


/* ************************************************************************** */
/* ************************************************************************** */
