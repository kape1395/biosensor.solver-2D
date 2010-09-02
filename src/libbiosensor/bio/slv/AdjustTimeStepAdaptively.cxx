
#include "AdjustTimeStepByFactor.hxx"

#include "AdjustTimeStepAdaptively.hxx"
#include "../Exception.hxx"
#include <iostream>
#include <cmath>
#include "../io/ConcentrationProfileReader.hxx"
#include "../Logging.hxx"
#define LOGGER "libbiosensor::AdjustTimeStepAdaptively: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::AdjustTimeStepAdaptively::AdjustTimeStepAdaptively(
    ISolver* solver,
    double factor,
    long adjustEveryNumberOfSteps,
    double maxTimeStep,
    std::vector<ISolverListener*>& stopConditions
) : AdjustTimeStepByFactor(solver, factor, adjustEveryNumberOfSteps, maxTimeStep)
{
    this->stopConditions = stopConditions;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::AdjustTimeStepAdaptively::~AdjustTimeStepAdaptively()
{
    //  Nothing to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_SLV_NS::AdjustTimeStepAdaptively::getNewTimeStep()
{
    //
    //  Check if solver not failed.
    //
    for (std::vector<ISolverListener*>::iterator sc = stopConditions.begin(); sc < stopConditions.end(); sc++)
    {
        (*sc)->solveEventOccured();
    }

    BIO_SLV_NS::IIterativeSolver* is = getIterativeSolver();
    if (is->isStopped() && !getSolver()->isSteadyStateReached())
    {
        // Failed, roll back in time.
        BIO_IO_NS::ConcentrationProfileReader* reader = concentrationWriter->createReaderForLastOutput();
        double prevTime = is->getSolvedTime();
        double prevTimeStep = is->getTimeStep();
        long   prevIteration = is->getSolvedIterationCount();
        double newTime = reader->getTime();
        double newTimeStep = prevTimeStep / getStepIncreaseFactor();

        LOG_WARN(LOGGER
                << "getNewTimeStep: "
                << "Failure reported, doing rollback in time. Updating"
                << " time(" << prevTime << "->" << newTime
                << ") iteration(" << prevIteration << "->" << reader->getIteration()
                << ") timeStep(" << prevTimeStep << "->" << newTimeStep << ")."
                );

        getSolver()->setState(reader);
        delete reader;

        //
        //  Schedule next time adjustment not before failure time.
        //
        scheduleNextAdjustment(std::ceil((prevTime - newTime) / newTimeStep));

        return newTimeStep;
    }
    else
    {
        return AdjustTimeStepByFactor::getNewTimeStep();
    }
}

/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::AdjustTimeStepAdaptively::changeTimeStep(double newTimeStep)
{
    //
    //  Store current state.
    //
    concentrationWriter->solveEventOccured();

    //
    //  Do usual things.
    //
    AdjustTimeStepByFactor::changeTimeStep(newTimeStep);
}

/* ************************************************************************** */
/* ************************************************************************** */
