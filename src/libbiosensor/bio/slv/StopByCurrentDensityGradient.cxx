#include "StopByCurrentDensityGradient.hxx"
#include "../Exception.hxx"
#include <iostream>
#include <cmath>
#include "../Logging.hxx"
#define LOGGER "libbiosensor::StopByCurrentDensityGradient: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::StopByCurrentDensityGradient::StopByCurrentDensityGradient(
    ISolver* solver,
    double gradient,
    bool normalized,
    long checkEveryNumberOfSteps
)
{
    this->iterativeSolver = dynamic_cast<IIterativeSolver*>(solver);
    this->transducer = solver->getTransducer();
    this->gradient = gradient;
    this->normalized = normalized;
    this->checkEveryNumberOfSteps = checkEveryNumberOfSteps;

    if (this->checkEveryNumberOfSteps <= 0)
        this->checkEveryNumberOfSteps = 1;

    if (iterativeSolver == 0)
        throw Exception("StopByCurrentDensityGradient: Solver must implement IIterativeSolver");

    this->nextStepForCheck = checkEveryNumberOfSteps;
    this->prevCurrentDensity = 0.0;
    this->prevTime = 0.0;

    this->nextStepListener = new NextStepListener(this);
    this->iterativeSolver->addListener(this->nextStepListener, false);
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::StopByCurrentDensityGradient::~StopByCurrentDensityGradient()
{
    delete nextStepListener;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::StopByCurrentDensityGradient::solveEventOccured()
{
    if (iterativeSolver->getSolvedIterationCount() >= nextStepForCheck)
    {
        //
        //  Sabe needed info at this step, later in the next step
        //  it will be used in the method processRequestedNextIteration().
        //
        prevCurrentDensity  = transducer->getOutput();
        prevTime            = iterativeSolver->getSolvedTime();
        nextStepForCheck    = iterativeSolver->getSolvedIterationCount() + checkEveryNumberOfSteps;

        nextStepListener->listenForNextStep(iterativeSolver->getSolvedIterationCount());
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::StopByCurrentDensityGradient::processNextStep()
{
    double thisCurrentDensity = transducer->getOutput();
    double thisTime = iterativeSolver->getSolvedTime();

    double grad = (thisCurrentDensity - prevCurrentDensity) / (thisTime - prevTime);
    double gradNormalized = grad * thisTime / thisCurrentDensity;

    std::cout << "DEBUG: StopByCurrentDensityGradient: grad=" << grad << "\tgradNormalized=" << gradNormalized << std::endl;

    if (std::abs(normalized ? gradNormalized : grad) < gradient)
    {
        iterativeSolver->stop(true);
        LOG_INFO(LOGGER << "Solver reached a steady state");
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
