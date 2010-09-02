#include "AdjustTimeStepAdaptively.hxx"
#include "IIterativeSolver.hxx"
#include "AdjustTimeStepByFactor.hxx"
#include "../Exception.hxx"
#include "../io/ConcentrationProfileReader.hxx"
#include "../Logging.hxx"
#include <iostream>
#include <cmath>
#define LOGGER "libbiosensor::AdjustTimeStepAdaptively: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::AdjustTimeStepAdaptively::AdjustTimeStepAdaptively(
    ISolver* solver,
    double  increaseFactor,
    long    increaseEveryStepCount,
    double  increaseMaxTimeStep,
    double  fallbackFactor,
    long    fallbackCheckEveryStepCount,
    double  fallbackMinTimeStep,
    BIO_IO_NS::ConcentrationProfile* concentrationWriter,
    std::vector<BIO_SLV_NS::ISolverListener*>& stopConditions
)
{
    this->solver = solver;
    this->iterativeSolver = dynamic_cast<BIO_SLV_NS::IIterativeSolver*>(solver);
    if (!iterativeSolver)
    {
        throw new BIO_NS::Exception("AdjustTimeStepAdaptively: solver must implement IIterativeSolver");
    }

    this->increaseFactor = increaseFactor;
    this->increaseEveryStepCount = increaseEveryStepCount;
    this->increaseMaxTimeStep = increaseMaxTimeStep;

    this->fallbackFactor = fallbackFactor;
    this->fallbackCheckEveryStepCount = fallbackCheckEveryStepCount;
    this->fallbackMinTimeStep = fallbackMinTimeStep;

    this->concentrationWriter = concentrationWriter;
    this->concentrationWriter->setOverwrite(true);
    this->stopConditions = stopConditions;

    this->stateSaved = false;
    this->failTime = 0;

    resetInternal();
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::AdjustTimeStepAdaptively::~AdjustTimeStepAdaptively()
{
    typedef std::vector<BIO_SLV_NS::ISolverListener*>::iterator ISLIyerator;
    for (ISLIyerator it = stopConditions.begin(); it < stopConditions.end(); it++)
    {
        delete *it;
    }
    delete concentrationWriter;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::AdjustTimeStepAdaptively::reset()
{
    resetInternal();
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::AdjustTimeStepAdaptively::resetInternal()
{
    nextIterationForFallbackCheck = iterativeSolver->getSolvedIterationCount();
    nextIterationForIncrease = iterativeSolver->getSolvedIterationCount() + increaseEveryStepCount;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::AdjustTimeStepAdaptively::solveEventOccured()
{
    if (isTimeForFallbackCheck())
    {
        if (isFallbackNeeded())
        {
            failTime = std::max(failTime, iterativeSolver->getSolvedTime());
            performFallback();
        }
        else
        {
            if (!isStateSaved() || isTimeForIncrease())
            {
                saveCurrentState();
            }
        }
        scheduleNextFallbackCheckAfter(fallbackCheckEveryStepCount);
    }

    if (isTimeForIncrease())
    {
        increaseTimeStep();
        scheduleNextIncreaseAfter(increaseEveryStepCount);
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
bool BIO_SLV_NS::AdjustTimeStepAdaptively::isFallbackNeeded()
{
    BIO_SLV_NS::IIterativeSolver* is = iterativeSolver;

    //
    //  Are we alredy failed? If yes, fallback will not help.
    //
    if (is->isStopped() && !solver->isSteadyStateReached())
    {
        LOG_WARN(LOGGER << "Solver already failed. Nothing to do here.");
        return false;
    }

    //
    //  Execurre all validators, and check solver's state
    //
    for (std::vector<ISolverListener*>::iterator sc = stopConditions.begin(); sc < stopConditions.end(); sc++)
    {
        (*sc)->solveEventOccured();
    }
    return is->isStopped() && !solver->isSteadyStateReached();
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::AdjustTimeStepAdaptively::performFallback()
{
    BIO_SLV_NS::IIterativeSolver* is = iterativeSolver;

    //  get current state
    double prevTime = is->getSolvedTime();
    double prevTimeStep = is->getTimeStep();
    long   prevIteration = is->getSolvedIterationCount();
    if (prevTimeStep <= fallbackMinTimeStep)
    {
        throw new BIO_NS::Exception(
            "AdjustTimeStepAdaptively::performFallback: Minimal time step was reached. Failing."
        );
    }

    // Read saved state
    BIO_IO_NS::ConcentrationProfileReader* reader = concentrationWriter->createReaderForLastOutput();
    if (!reader)
    {
        throw new BIO_NS::Exception(
            "AdjustTimeStepAdaptively::performFallback: Unable to read last state. There was no output before?"
        );
    }
    double newTime = reader->getTime();
    double newTimeStep = std::max(prevTimeStep / fallbackFactor, fallbackMinTimeStep);

    // Do fallback.
    LOG_WARN(LOGGER
             << "performFallback: Updating"
             << " time(" << prevTime << "->" << newTime
             << ") iteration(" << prevIteration << "->" << reader->getIteration()
             << ") timeStep(" << prevTimeStep << "->" << newTimeStep << ")."
            );
    solver->setState(reader);
    is->setTimeStep(newTimeStep);
    is->resume();
    delete reader;

    // Schedule next step increase
    scheduleNextIncreaseAfter(
        std::ceil((std::max(failTime, prevTime) - newTime) / newTimeStep)
    );
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::AdjustTimeStepAdaptively::saveCurrentState()
{
    LOG_INFO(LOGGER
             << "Saving current state. Current iteration is "
             << iterativeSolver->getSolvedIterationCount()
            );
    concentrationWriter->solveEventOccured();
    stateSaved = true;
}


/* ************************************************************************** */
/* ************************************************************************** */
bool BIO_SLV_NS::AdjustTimeStepAdaptively::isStateSaved()
{
    return stateSaved;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::AdjustTimeStepAdaptively::increaseTimeStep()
{
    double newTimeStep = std::min(iterativeSolver->getTimeStep() * increaseFactor, increaseMaxTimeStep);
    if (iterativeSolver->getTimeStep() <= newTimeStep)
    {
        LOG_INFO(LOGGER << "increaseTimeStep:"
                 << " timeStep(" << iterativeSolver->getTimeStep() << "->" << newTimeStep
                 << ") at: solvedTime=" << iterativeSolver->getSolvedTime()
                 << " solvedIterationCount=" << iterativeSolver->getSolvedIterationCount()
                );
        iterativeSolver->setTimeStep(newTimeStep);
    }
    else
    {
        LOG_INFO(LOGGER << "increaseTimeStep: Maximal time step is reached. Leaving it as is:"
                 << " timeStep=" << iterativeSolver->getTimeStep()
                 << " solvedTime=" << iterativeSolver->getSolvedTime()
                 << " solvedIterationCount=" << iterativeSolver->getSolvedIterationCount()
                );
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::AdjustTimeStepAdaptively::scheduleNextIncreaseAfter(long stepCount)
{
    nextIterationForIncrease = std::max(
                                   nextIterationForIncrease,
                                   iterativeSolver->getSolvedIterationCount() + stepCount
                               );
    LOG_DEBUG(LOGGER
              << "Scheduling next step increase at "
              << nextIterationForIncrease
              << " current iteration is "
              << iterativeSolver->getSolvedIterationCount()
             );
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::AdjustTimeStepAdaptively::scheduleNextFallbackCheckAfter(long stepCount)
{
    nextIterationForFallbackCheck = iterativeSolver->getSolvedIterationCount() + stepCount;
    LOG_DEBUG(LOGGER
              << "Scheduling next fallbackCheck at "
              << nextIterationForFallbackCheck
              << " current iteration is "
              << iterativeSolver->getSolvedIterationCount()
             );
}


/* ************************************************************************** */
/* ************************************************************************** */
bool BIO_SLV_NS::AdjustTimeStepAdaptively::isTimeForFallbackCheck()
{
    return iterativeSolver->getSolvedIterationCount() >= nextIterationForFallbackCheck;
}


/* ************************************************************************** */
/* ************************************************************************** */
bool BIO_SLV_NS::AdjustTimeStepAdaptively::isTimeForIncrease()
{
    return iterativeSolver->getSolvedIterationCount() >= nextIterationForIncrease;
}


/* ************************************************************************** */
/* ************************************************************************** */
