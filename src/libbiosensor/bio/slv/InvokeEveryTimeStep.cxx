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
    this->nextStopByStep = 0;
    this->nextStopByTime = 0.0;

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
        nextStopByStep += stepByStep;
        nextStopByTime += stepByTime;
    }
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
