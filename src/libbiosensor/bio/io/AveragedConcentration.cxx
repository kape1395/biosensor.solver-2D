/*
 * Copyright 2011 Karolis Petrauskas
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#include "AveragedConcentration.hxx"
#include "../Logging.hxx"
#include "../Exception.hxx"
#include "../trd/IntegratedConcentration.hxx"
#include <iostream>
#include <cmath>
#define LOGGER "libbiosensor::AveragedConcentration: "

/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::AveragedConcentration::AveragedConcentration(
    std::string& name,
    BIO_SLV_NS::ISolver* solver,
    BIO_IO_NS::IContext* context,
    BIO_XML_MODEL_NS::MediumName* medium
)
{
    LOG_DEBUG(LOGGER << "AveragedConcentration()...");

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

            LOG_DEBUG(LOGGER << "Creating IntegralOverArea for"
                      << " substance[" << *subst << "]=" << subsConfig->name()
                      << " over medium=" << *medium
                     );

            substances.push_back(subsConfig);
            integrals.push_back(new BIO_TRD_NS::IntegralOverArea(
                                    solver,
                                    *medium,
                                    new BIO_TRD_NS::IntegratedConcentration(structAnalyzer, subsConfig->name()),
                                    structAnalyzer
                                ));
        }
    }
    else
    {
        std::vector<BIO_XML_MODEL_NS::Substance*> substs = structAnalyzer->getSubstances();
        for (std::vector<BIO_XML_MODEL_NS::Substance*>::iterator subst = substs.begin(); subst < substs.end(); subst++)
        {
            LOG_DEBUG(LOGGER << "Creating IntegralOverArea for"
                      << " substance=" << (*subst)->name()
                      << " over all model");

            substances.push_back(*subst);
            integrals.push_back(new BIO_TRD_NS::IntegralOverArea(
                                    solver,
                                    new BIO_TRD_NS::IntegratedConcentration(structAnalyzer, (*subst)->name()),
                                    structAnalyzer
                                ));
        }
    }

    LOG_DEBUG(LOGGER << "AveragedConcentration()... Done");
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
        delete (*integral)->getExpression();
        delete *integral;
    }
    integrals.clear();

    delete structAnalyzer;

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
