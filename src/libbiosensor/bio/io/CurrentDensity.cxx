#include "CurrentDensity.hxx"
#include "../Exception.hxx"
#include <iostream>

/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::CurrentDensity::CurrentDensity(
    std::string& name,
    BIO_SLV_NS::ISolver* solver,
    BIO_IO_NS::IContext* Context
)
{
    this->name = name;
    this->solver = solver;
    this->Context = Context;
    this->output = 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::CurrentDensity::~CurrentDensity()
{
    if (output)
        Context->close(output);
    output = 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_IO_NS::CurrentDensity::solveEventOccured()
{
    if (!output)
    {
        output = Context->getOutputStream(name);
        output->precision(12);
        (*output) << "# Time\tStep\tCurrentDensity" << std::endl;
    }

    BIO_SLV_NS::IIterativeSolver* iterativeSolver = dynamic_cast<BIO_SLV_NS::IIterativeSolver*>(solver);
    if (iterativeSolver)
    {
        (*output) << iterativeSolver->getSolvedTime() << '\t' << iterativeSolver->getSolvedIterationCount();
    }
    else
    {
        (*output) << '\t';
    }

    (*output) << "\t" << solver->getTransducer()->getOutput() << std::endl;
}


/* ************************************************************************** */
/* ************************************************************************** */
