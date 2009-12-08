#include "InvokeNotBefore.hxx"
#include "../Exception.hxx"

/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::InvokeNotBefore::InvokeNotBefore(
    ISolver* solver
)
{
    this->solver = dynamic_cast<IIterativeSolver*>(solver);
    this->stepCount = 0;
    this->time = 0.0;

    if (this->solver == 0)
        throw Exception("InvokeNotBefore: Solver must implement IIterativeSolver");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::InvokeNotBefore::~InvokeNotBefore()
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
void BIO_SLV_NS::InvokeNotBefore::addListener(
    BIO_SLV_NS::ISolverListener* listener,
    bool deleteAtDestruction
)
{
    listeners.push_back(listener);
    if (deleteAtDestruction)
    {
        listenersToDelete.push_back(listener);
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::InvokeNotBefore::solveEventOccured()
{
    if (    (stepCount != 0 && solver->getSolvedIterationCount() >= stepCount) ||
            (time      != 0 && solver->getSolvedTime()           >= time     )
       )
    {
        for (SLVector::iterator l = listeners.begin(); l < listeners.end(); l++)
        {
            (*l)->solveEventOccured();
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::InvokeNotBefore::reset()
{
    for (SLVector::iterator l = listeners.begin(); l < listeners.end(); l++)
        (*l)->reset();
}


/* ************************************************************************** */
/* ************************************************************************** */
