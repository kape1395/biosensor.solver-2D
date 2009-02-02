#include "StopAtSpecifiedPoint.hxx"
#include "../Exception.hxx"

/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::StopAtSpecifiedPoint::StopAtSpecifiedPoint(
    ISolver* solver
) : log(log4cxx::Logger::getLogger("libbiosensor.StopAtSpecifiedPoint"))
{
    this->solver = dynamic_cast<IIterativeSolver*>(solver);
    this->stepCount = 0;
    this->time = 0.0;

    if (this->solver == 0)
        throw Exception("StopAtSpecifiedPoint: Solver must implement IIterativeSolver");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::StopAtSpecifiedPoint::~StopAtSpecifiedPoint()
{
    //  Nothing to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::StopAtSpecifiedPoint::solveEventOccured()
{
    if (!solver->isStopped() && (
                (stepCount != 0 && solver->getSolvedIterationCount() >= stepCount) ||
                (time      != 0 && solver->getSolvedTime()           >= time     )
            ))
    {
        solver->stop();
        LOG4CXX_INFO(log, "Solver is stopped by request at a specified time or stepCount.");
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
