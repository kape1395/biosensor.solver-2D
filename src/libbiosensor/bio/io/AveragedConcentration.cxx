#include "AveragedConcentration.hxx"
#include "../Exception.hxx"
#include <iostream>
#include <cmath>


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
    this->medium = medium;
    this->output = 0;
    this->structAnalyzer = new BIO_CFG_NS::StructureAnalyzer(solver->getConfig());

    if (medium)
    {
        std::vector<int> substs = structAnalyzer->getSubstanceIndexesInMedium(*medium);
        for (std::vector<int>::iterator subst = substs.begin(); subst < substs.end(); subst++)
        {
            BIO_XML_MODEL_NS::Substance* subsConfig = structAnalyzer->getSubstances()[*subst];
            substances.push_back(subsConfig);
            integrals.push_back(new BIO_TRD_NS::ConcentrationIntegralOverArea(
                                    solver,
                                    *medium,
                                    subsConfig->name(),
                                    structAnalyzer
                                ));
        }
    }
    else
    {
        std::vector<BIO_XML_MODEL_NS::Substance*> substs = structAnalyzer->getSubstances();
        for (std::vector<BIO_XML_MODEL_NS::Substance*>::iterator subst = substs.begin(); subst < substs.end(); subst++)
        {
            substances.push_back(*subst);
            integrals.push_back(new BIO_TRD_NS::ConcentrationIntegralOverArea(
                                    solver,
                                    (*subst)->name(),
                                    structAnalyzer
                                ));
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::AveragedConcentration::~AveragedConcentration()
{
    if (output)
    {
        context->close(output);
    }
    output = 0;

    for (Integrals::iterator integral = integrals.begin(); integral < integrals.end(); integral++)
    {
        delete *integral;
    }
    integrals.clear();

    substances.clear();
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_IO_NS::AveragedConcentration::solveEventOccured()
{
    if (!output)
    {
        output = context->getOutputStream(name);

        //  Print header.
        (*output) << "# Time\tStep";
        for (Substances::iterator s = substances.begin(); s < substances.end(); s++)
        {
            (*output) << '\t' << (*s)->name();
        }
        (*output) << std::endl;
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

    for (Integrals::iterator i = integrals.begin(); i < integrals.end(); i++)
    {
        (*output) << '\t' << ((*i)->integrateOverVolume() / (*i)->getVolume());
    }
    (*output) << std::endl;
}


/* ************************************************************************** */
/* ************************************************************************** */
