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
#include "Solver.hxx"
#include <bio/Exception.hxx>
#include <bio/Logging.hxx>
#include "AreaSubSolver.hxx"
#define LOGGER "libbiosensor-slv-fd::im1d::Solver: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM1D_NS::Solver::Solver(
    BIO_XML_NS::model::Model* config,
    BIO_NS::IFactory* factory,
    BIO_SLV_FD_IM2D_NS::ISubSolverFactory* subSolverFactory
) : BIO_SLV_FD_IM2D_NS::Solver::Solver(config, factory, subSolverFactory)
{
    //  TODO: Implement.
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM1D_NS::Solver::~Solver()
{
    //  TODO: Implement.
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM1D_NS::Solver::solveIteration()
{
    LOG_DEBUG(LOGGER << "solveIteration(" << getSolvedIterationCount() << ")...");

    BIO_SLV_FD_IM1D_NS::Solver::SplittedSolver* subSolvers = getSubSolvers();

    LOG_TRACE(LOGGER << "solveIteration: Solve forward");
    for (int v = 0; v <= subSolvers->sizeV(); v++)   // Forward
    {
        bool lastV = v == subSolvers->sizeV();

        subSolvers->getBoundsH()[0][v]->solveThroughForward();

        if (!lastV)
        {
            subSolvers->getAreas()[0][v]->solveVerticalForward();
        }
    }


    LOG_TRACE(LOGGER << "solveIteration: Solve backward");
    for (int v = subSolvers->sizeV(); v >= 0; v--)  // Backward
    {
        subSolvers->getBoundsH()[0][v]->solveThroughBackward();

        if (v > 0)
        {
            subSolvers->getAreas()[0][v - 1]->solveVerticalBackward();
        }
    }

    LOG_TRACE(LOGGER << "solveIteration()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
