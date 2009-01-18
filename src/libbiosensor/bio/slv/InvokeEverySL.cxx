#include "InvokeEverySL.hxx"
#include "../Exception.hxx"

/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::InvokeEverySL::InvokeEverySL(
    ISolver* solver,
    long stepCount
) : log(log4cxx::Logger::getLogger("libbiosensor.InvokeEverySL"))
{
    this->solver = dynamic_cast<IIterativeSolver*>(solver);
    this->stepCount = stepCount;
    this->nextStop = 0;

    if (this->solver == 0)
        throw Exception("StopAtStepSL: Solver must implement IIterativeSolver");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::InvokeEverySL::~InvokeEverySL()
{
    //  Nothing to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::InvokeEverySL::solveEventOccured()
{
    if (solver->getSolvedIterationCount() >= nextStop)
    {
        for (int i = 0; i < listeners.size(); i++)
        {
            listeners[i]->solveEventOccured();
        }
        nextStop += stepCount;
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::InvokeEverySL::addListener(ISolverListener* listener)
{
    this->listeners.push_back(listener);
}


/* ************************************************************************** */
/* ************************************************************************** */
