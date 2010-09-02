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

    this->nextStepForAdjustment = iterativeSolver->getSolvedIterationCount() + adjustEveryNumberOfSteps;
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
        double newTimeStep = getNewTimeStep();

        if (newTimeStep != iterativeSolver->getTimeStep())
        {
            changeTimeStep(newTimeStep);
        }

        reset();
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::AdjustTimeStepByFactor::reset()
{
    scheduleNextAdjustment(adjustEveryNumberOfSteps);
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
                << " At iteration=" << requested
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
             << " solvedTime=" << iterativeSolver->getSolvedTime()
             << " solvedIterationCount=" << iterativeSolver->getSolvedIterationCount()
            );
    iterativeSolver->setTimeStep(newTimeStep);
}


/* ************************************************************************** */
/* ************************************************************************** */
