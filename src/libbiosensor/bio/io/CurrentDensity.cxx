#include "CurrentDensity.hxx"
#include "../Exception.hxx"
#include <iostream>

/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::CurrentDensity::CurrentDensity(
    std::string& name,
    BIO_SLV_NS::ISolver* solver,
    BIO_IO_NS::IOutputContext* outputContext
) : log(log4cxx::Logger::getLogger("libbiosensor.CurrentDensity"))
{
    this->name = name;
    this->solver = solver;
    this->outputContext = outputContext;
    this->output = 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::CurrentDensity::~CurrentDensity()
{
    if (output)
        outputContext->close(output);
    output = 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_IO_NS::CurrentDensity::solveEventOccured()
{
    if (!output)
    {
        output = outputContext->getOutputStream(name);
        (*output) << "# Iteration\tTime\tCurrentDensity" << std::endl;
    }

    BIO_SLV_NS::IIterativeSolver* iterativeSolver = dynamic_cast<BIO_SLV_NS::IIterativeSolver*>(solver);
    if (iterativeSolver)
    {
        (*output) << iterativeSolver->getSolvedIterationCount() << '\t' << iterativeSolver->getSolvedTime();
    }
    else
    {
        (*output) << '\t';
    }

    (*output) << "\t" << solver->getTransducer()->getOutput() << std::endl;
}


/* ************************************************************************** */
/* ************************************************************************** */
