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
    this->iterativeSolver = dynamic_cast<IIterativeSolver*>(solver);
    this->factor = factor;
    this->adjustEveryNumberOfSteps = adjustEveryNumberOfSteps;
    this->maxTimeStep = maxTimeStep;

    if (this->adjustEveryNumberOfSteps <= 0)
        throw Exception("AdjustTimeStepByFactor: adjustEveryNumberOfSteps must be > 0.");

    if (iterativeSolver == 0)
        throw Exception("AdjustTimeStepByFactor: Solver must implement IIterativeSolver");

    this->nextStepForAdjustment = adjustEveryNumberOfSteps;
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
        double newTimeStep = iterativeSolver->getTimeStep() * factor;
        if (maxTimeStep > 0.0 && newTimeStep > maxTimeStep)
        {
            newTimeStep = maxTimeStep;
        }
        if (newTimeStep != iterativeSolver->getTimeStep())
        {
            LOG_INFO(LOGGER << "Changing time step."
                     << " oldTimeStep=" << iterativeSolver->getTimeStep()
                     << " newTimeStep=" << newTimeStep
                     << " solvedTime=" << iterativeSolver->getSolvedTime()
                     << " solvedIterationCount=" << iterativeSolver->getSolvedIterationCount()
                    );
            iterativeSolver->setTimeStep(newTimeStep);
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
