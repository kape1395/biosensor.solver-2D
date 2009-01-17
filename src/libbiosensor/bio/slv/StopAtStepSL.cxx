#include "StopAtStepSL.hxx"
#include "../Exception.hxx"

/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::StopAtStepSL::StopAtStepSL(
    ISolver* solver,
    long stepNumber
) : log(log4cxx::Logger::getLogger("libbiosensor::StopAtStepSL"))
{
    this->solver = dynamic_cast<IIterativeSolver*>(solver);
    this->stepNumber = stepNumber;

    if (this->solver == 0)
        throw Exception("StopAtStepSL: Solver must implement IIterativeSolver");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::StopAtStepSL::~StopAtStepSL()
{
    //  Nothing to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::StopAtStepSL::solveEventOccured()
{
    if (solver->getSolvedIterationCount() >= stepNumber && !solver->isStopped())
    {
        solver->stop();
        LOG4CXX_INFO(log, "Solver is stopped by request at a specified step.");
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
