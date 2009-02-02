#include "ConcentrationProfile.hxx"
#include "../Exception.hxx"
#include <iostream>

/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::ConcentrationProfile::ConcentrationProfile(
    std::string& name,
    long indexed,
    BIO_SLV_NS::ISolver* solver,
    BIO_IO_NS::IOutputContext* outputContext
) : log(log4cxx::Logger::getLogger("libbiosensor.ConcentrationProfile"))
{
    this->name = name;
    this->indexed = indexed;
    this->currentIndex = 0;
    this->solver = solver;
    this->outputContext = outputContext;

    if ((this->grid = dynamic_cast<BIO_DM_NS::IGrid2D*>(solver->getData())) == 0)
    {
        throw Exception("ConcentrationProfile: IGrid2D DataModel is required");
    }
    this->cursor = this->grid->newGridCursor();
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::ConcentrationProfile::~ConcentrationProfile()
{
    delete cursor;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_IO_NS::ConcentrationProfile::solveEventOccured()
{
    using BIO_SLV_NS::IIterativeSolver;
    int substCount = grid->getSubstanceCount();

    std::ostream* out = indexed
                        ? outputContext->getOutputStream(name, currentIndex)
                        : outputContext->getOutputStream(name);


    IIterativeSolver* iterativeSolver = dynamic_cast<IIterativeSolver*>(solver);
    if (iterativeSolver != 0)
    {
        (*out) << "#"
        << " SolvedIterationCount=" << iterativeSolver->getSolvedIterationCount()
        << " SolvedTime="           << iterativeSolver->getSolvedTime()
        << std::endl;
    }
    else
    {
        (*out) << "# SolvedIterationCount=? SolvedTime=?" << std::endl;
    }

    (*out) << "# h\tv";
    for (int s = 0; s < substCount; s++)
    {
        (*out) << '\t' << grid->getSubstanceConf(s)->name();
    }
    (*out) << std::endl;

    int h;
    int v;
    cursor->colStart();
    cursor->rowStart();
    for (v = 0; cursor->isValid(); v++, cursor->down())
    {
        for (h = 0; cursor->isValid(); h++, cursor->right())
        {
            BIO_DM_NS::IConcentrations* concentrations = cursor->getConcentrations();

            (*out) << h << '\t' << v;
            for (int s = 0; s < substCount; s++)
            {
                (*out) << '\t' << (*concentrations)[s];
            }
            (*out) << std::endl;
        }
        cursor->rowStart();
    }

    outputContext->close(out);
    currentIndex++;
}


/* ************************************************************************** */
/* ************************************************************************** */
