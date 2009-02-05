#include "ConcentrationProfile.hxx"
#include "../Exception.hxx"
#include <iostream>
#include <sstream>
#include <cmath>

/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::ConcentrationProfile::ConcentrationProfile(
    std::string& name,
    BIO_SLV_NS::ISolver* solver,
    BIO_IO_NS::IContext* Context
) : log(log4cxx::Logger::getLogger("libbiosensor.ConcentrationProfile"))
{
    this->name = name;
    this->indexed = false;
    this->currentIndex = 0;
    this->solver = solver;
    this->Context = Context;

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
                        ? Context->getOutputStream(name, currentIndex)
                        : Context->getOutputStream(name);


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

    std::stringstream header;
    header << "# h\tv";
    for (int s = 0; s < substCount; s++)
    {
        header << '\t' << grid->getSubstanceConf(s)->name();
    }

    int h;
    int v;
    cursor->colStart();
    cursor->rowStart();
    for (v = 0; cursor->isValid(); v++, cursor->down())
    {
        (*out) << std::endl << header.str() << std::endl;
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

    Context->close(out);
    currentIndex++;
}


/* ************************************************************************** */
/* ************************************************************************** */
