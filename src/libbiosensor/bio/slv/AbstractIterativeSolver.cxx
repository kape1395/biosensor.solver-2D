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
#include "AbstractIterativeSolver.hxx"
#include "ISolverListener.hxx"
#include "../Logging.hxx"
#define LOGGER "libbiosensor::AbstractIterativeSolver: "

BIO_SLV_NS_BEGIN


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Constructor.
 */
AbstractIterativeSolver::AbstractIterativeSolver(
    BIO_XML_NS::model::Model* config
) : AbstractSolver(config), IIterativeSolver()
{
    timeStep = 0;
    timeSolved = 0;
    iterationsSolved = 0;
    stopped = true;
    steadyStateReached = false;
    listeners.clear();
    listenersToDelete.clear();
}


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Destructor.
 */
AbstractIterativeSolver::~AbstractIterativeSolver()
{
    // nothing
    for (unsigned i = 0; i < listenersToDelete.size(); i++)
    {
        delete listenersToDelete[i];
    }
    listeners.clear();
    listenersToDelete.clear();
}


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Solve model.
 *  TODO: Can it be invoked after stop?
 */
void AbstractIterativeSolver::solve()
{
    stopped = false;
    invokeListeners();
    while (!stopped)
    {
        solveIteration();
        iterationsSolved++;
        timeSolved += timeStep;
        invokeListeners();
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Stop the solver.
 */
void AbstractIterativeSolver::stop(bool steadyStateReached)
{
    this->stopped = true;
    this->steadyStateReached = steadyStateReached;
    //invokeListeners();    FIXME: I think this is not needed or must be reported as different type of efent.
}


/* ************************************************************************** */
/* ************************************************************************** */
void AbstractIterativeSolver::resume()
{
    this->stopped = false;
    this->steadyStateReached = false;
}


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Is solved stopped?
 */
bool AbstractIterativeSolver::isStopped()
{
    return this->stopped;
}


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Is steady state reached?
 */
bool AbstractIterativeSolver::isSteadyStateReached()
{
    return this->steadyStateReached;
}


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Return time step.
 */
double AbstractIterativeSolver::getTimeStep()
{
    return this->timeStep;
}


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Set new time step.
 */
void AbstractIterativeSolver::setTimeStep(double timeStep)
{
    this->timeStep = timeStep;
}


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Return solved iterations count.
 */
long AbstractIterativeSolver::getSolvedIterationCount()
{
    return this->iterationsSolved;
}


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Return solved time.
 */
double AbstractIterativeSolver::getSolvedTime()
{
    return this->timeSolved;
}


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Register new listener.
 */
void AbstractIterativeSolver::addListener(
    ISolverListener* listener,
    bool deleteOnDestruction
)
{
    this->listeners.push_back(listener);
    if (deleteOnDestruction)
        this->listenersToDelete.push_back(listener);
}


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Invoke all registered listeners.
 */
void AbstractIterativeSolver::invokeListeners()
{
    LOG_TRACE(LOGGER << "invokeListeners()...");
    for (unsigned i = 0; i < listeners.size(); i++)
    {
        LOG_TRACE(LOGGER << "invokeListeners() listener[" << i << "]...");
        listeners[i]->solveEventOccured();
        LOG_TRACE(LOGGER << "invokeListeners() listener[" << i << "]... Done");
    }
    LOG_TRACE(LOGGER << "invokeListeners()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Resets all listeners.
 */
void AbstractIterativeSolver::resetListeners()
{
    for (unsigned i = 0; i < listeners.size(); i++)
    {
        listeners[i]->reset();
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Set the current state. Used to implement resume functionality.
 */
void AbstractIterativeSolver::setIterationState(long solvedIterationCount, double solvedTime, double timeStep)
{
    this->iterationsSolved = solvedIterationCount;
    this->timeSolved = solvedTime;
    this->setTimeStep(timeStep);
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS_END
