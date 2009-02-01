#include "InvokeNotBefore.hxx"
#include "../Exception.hxx"

/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::InvokeNotBefore::InvokeNotBefore(
    ISolver* solver
) : log(log4cxx::Logger::getLogger("libbiosensor::InvokeNotBefore"))
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
    //  Nothing to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::InvokeNotBefore::solveEventOccured()
{
    if (    (stepCount != 0 && solver->getSolvedIterationCount() >= stepCount) ||
            (time      != 0 && solver->getSolvedTime()           >= time     )
       )
    {
        for (std::vector<BIO_SLV_NS::ISolverListener*>::iterator l = listeners.begin();
                l < listeners.end(); l++)
        {
            (*l)->solveEventOccured();
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
