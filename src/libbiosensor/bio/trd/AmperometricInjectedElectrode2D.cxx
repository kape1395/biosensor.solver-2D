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
#include "AmperometricInjectedElectrode2D.hxx"
#include "../Logging.hxx"
#include "../Exception.hxx"
#include "../dm/ConstantSegmentSplit.hxx"
#include "../slv/IIterativeSolver.hxx"
#include "IntegratedReaction.hxx"
#define LOGGER "libbiosensor::AmperometricInjectedElectrode2D: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::AmperometricInjectedElectrode2D::AmperometricInjectedElectrode2D(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::MediumName& mediumName,
    BIO_XML_MODEL_NS::ReactionName& reactionName
)
{
    if (!dynamic_cast<BIO_SLV_NS::IIterativeSolver*>(solver))
        throw Exception("InjectedElectrode: Solver must implement IIterativeSolver.");

    if (!(dataModel = dynamic_cast<BIO_DM_NS::IComposite2D*>(solver->getData())))
        throw Exception("InjectedElectrode: DataModel must implement IComposite2D.");


    this->solver = solver;
    this->structAnalyzer = new BIO_CFG_NS::StructureAnalyzer(solver->getConfig());
    this->mediumName = mediumName;
    this->reactionName = reactionName;

    this->areaIntegrator = (new IntegralOverArea(
                                solver,
                                mediumName,
                                IntegratedReaction::newInstance(structAnalyzer, reactionName),
                                structAnalyzer
                            ))->forOpenArea(true);

    this->calculatedOutput = 0.0;
    this->calculatedOutputForStep = -1;
}




/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::AmperometricInjectedElectrode2D::~AmperometricInjectedElectrode2D()
{
    delete areaIntegrator->getExpression();
    delete areaIntegrator;
    delete structAnalyzer;
    areas.clear();
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_TRD_NS::AmperometricInjectedElectrode2D::getOutput()
{
    BIO_SLV_NS::IIterativeSolver* iterative = dynamic_cast<BIO_SLV_NS::IIterativeSolver*>(solver);
    if (calculatedOutputForStep == iterative->getSolvedIterationCount())
    {
        return calculatedOutput;
    }

    LOG_DEBUG(LOGGER << "getOutput()...");

    double integralValue = areaIntegrator->integrate();

    integralValue *= CONST_F * CONST_n_e;

    //
    //  Divide by surface.
    //
    if (structAnalyzer->isCoordinateSystemCylindrical())
    {
        //
        //   integrate by angle (\fi) and divide by an area of a circle:
        //      ((2 \pi) / (\pi r^2))
        //
        double cellRadius = structAnalyzer->getPointsH()[structAnalyzer->getPointsH().size() - 1]->value();
        integralValue *= 2.0 / (cellRadius * cellRadius);
    }
    else
    {
        //
        //  We are integrated all in one line, now just divide all by it`s lenght.
        //
        double areaWidth = structAnalyzer->getPointsH()[structAnalyzer->getPointsH().size() - 1]->value();
        integralValue /= areaWidth;
    }



    calculatedOutput = integralValue;
    calculatedOutputForStep = iterative->getSolvedIterationCount();

    LOG_DEBUG(LOGGER << "getOutput()... Done, result=" << integralValue);

    return integralValue;
}


/* ************************************************************************** */
/* ************************************************************************** */
