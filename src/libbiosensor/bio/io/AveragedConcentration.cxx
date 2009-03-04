#include "AveragedConcentration.hxx"
#include "../Exception.hxx"
#include <iostream>

/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::AveragedConcentration::AveragedConcentration(
    std::string& name,
    BIO_SLV_NS::ISolver* solver,
    BIO_IO_NS::IContext* context,
    BIO_XML_MODEL_NS::MediumName* medium
)
{
    this->name = name;
    this->solver = solver;
    this->context = context;
    this->output = 0;

    //  TODO: Implement
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::AveragedConcentration::~AveragedConcentration()
{
    if (output)
        context->close(output);
    output = 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_IO_NS::AveragedConcentration::solveEventOccured()
{
    if (!output)
    {
        output = context->getOutputStream(name);
        // (*output) << "# Time\tStep\tCurrentDensity" << std::endl;
    }

    /*
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
     */
}


/* ************************************************************************** */
/* ************************************************************************** */
