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
#include "AdjustTimeStepByFactor.hxx"
#include "../Exception.hxx"
#include <iostream>
#include <cmath>
#include "../Logging.hxx"
#define LOGGER "libbiosensor::AdjustTimeStepByFactor: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::AdjustTimeStepByFactor::AdjustTimeStepByFactor(
    ISolver* solver,
    double factor,
    long adjustEveryNumberOfSteps,
    double maxTimeStep
)
{
    this->solver = solver;
    this->iterativeSolver = dynamic_cast<IIterativeSolver*>(solver);
    this->factor = factor;
    this->adjustEveryNumberOfSteps = adjustEveryNumberOfSteps;
    this->maxTimeStep = maxTimeStep;

    if (this->adjustEveryNumberOfSteps <= 0)
        throw Exception("AdjustTimeStepByFactor: adjustEveryNumberOfSteps must be > 0.");

    if (iterativeSolver == 0)
        throw Exception("AdjustTimeStepByFactor: Solver must implement IIterativeSolver");

    this->nextStepForAdjustment = iterativeSolver->getSolvedIterationCount();
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::AdjustTimeStepByFactor::~AdjustTimeStepByFactor()
{
    //  Nothing to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::AdjustTimeStepByFactor::solveEventOccured()
{
    if (iterativeSolver->getSolvedIterationCount() >= nextStepForAdjustment)
    {
        LOG_DEBUG(LOGGER << "solveEventOccured...");
        double newTimeStep = getNewTimeStep();

        if (newTimeStep != iterativeSolver->getTimeStep())
        {
            changeTimeStep(newTimeStep);
        }

        scheduleNextAdjustment(adjustEveryNumberOfSteps);
        LOG_DEBUG(LOGGER << "solveEventOccured... Done");
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::AdjustTimeStepByFactor::reset()
{
    nextStepForAdjustment = iterativeSolver->getSolvedIterationCount() + iterativeSolver->getSolvedIterationCount();
}

/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::AdjustTimeStepByFactor::scheduleNextAdjustment(long stepCount)
{
    long current = iterativeSolver->getSolvedIterationCount();
    long requested = current + stepCount;
    if (nextStepForAdjustment < requested)
    {
        LOG_INFO(LOGGER
                 << "scheduleNextAdjustment: "
                 << "Current iteration=" << current
                 << ", next attempt will be at iteration=" << requested
                );

        nextStepForAdjustment = requested;
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_SLV_NS::AdjustTimeStepByFactor::getNewTimeStep()
{
    double newTimeStep = iterativeSolver->getTimeStep() * factor;

    if (maxTimeStep > 0.0 && newTimeStep > maxTimeStep)
    {
        newTimeStep = maxTimeStep;
    }

    return newTimeStep;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::AdjustTimeStepByFactor::changeTimeStep(double newTimeStep)
{
    LOG_INFO(LOGGER << "Changing time step."
             << " oldTimeStep=" << iterativeSolver->getTimeStep()
             << " newTimeStep=" << newTimeStep
             << " at: solvedTime=" << iterativeSolver->getSolvedTime()
             << " solvedIterationCount=" << iterativeSolver->getSolvedIterationCount()
            );
    iterativeSolver->setTimeStep(newTimeStep);
}


/* ************************************************************************** */
/* ************************************************************************** */
