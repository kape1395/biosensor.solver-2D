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
#include "bio/slv/AbstractIterativeSolver.hxx"
#include "Solver.hxx"
#include <bio/trd/AmperometricElectrode2DOnBound.hxx>
#include <bio/trd/AmperometricInjectedElectrode2D.hxx>
#include <bio/Exception.hxx>
#include <bio/Logging.hxx>
#define LOGGER "libbiosensor-slv-fd::im2d::Solver: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::Solver::Solver(
    BIO_XML_NS::model::Model* config,
    BIO_NS::IFactory* factory,
    BIO_SLV_FD_IM2D_NS::ISubSolverFactory* subSolverFactory
) : AbstractIterativeSolver(config)
{
    LOG_DEBUG(LOGGER << "Solver()...");

    solverState = new SolverStateImpl(this);

    structAnalyzer = new BIO_CFG_NS::StructureAnalyzer(config);
    boundAnalyzer = new BIO_CFG_NS::BoundAnalyzer(structAnalyzer);
    fdAnalyzer = new BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer(config);

    if (!structAnalyzer->isTwoDimensional())
    {
        LOG_ERROR(LOGGER << "Config is not two-dimensional, this solver supports only two-dimensional models");
        throw Exception("Unsuppordet model");
    }


    subSolvers = new SplittedSolver(
        structAnalyzer->getPointsH().size() - 1,
        structAnalyzer->getPointsV().size() - 1
    );

    LOG_DEBUG(LOGGER << "SubSolver grid has sizeH=" << subSolvers->sizeH() << " sizeV=" << subSolvers->sizeV());


    ////////////////////////////////////////////////////////////////////////////
    //  Initialize area solvers.
    //
    for (int h = 0; h <= subSolvers->sizeH(); h++)
    {
        for (int v = 0; v <= subSolvers->sizeV(); v++)
        {
            bool lastH = h == subSolvers->sizeH();
            bool lastV = v == subSolvers->sizeV();

            if (!(lastH || lastV))
                subSolvers->getAreas()[h][v] = subSolverFactory->createAreaSubSolver(this, h, v);

            if (!lastH)
                subSolvers->getBoundsH()[h][v] = new BoundSubSolver( // Horizontal Bound
                    this, h, v, true,
                    v > 0 ? subSolvers->getAreas()[h][v - 1] : 0,
                    !lastV ? subSolvers->getAreas()[h][v] : 0,
                    structAnalyzer, fdAnalyzer, boundAnalyzer
                );

            if (!lastV)
                subSolvers->getBoundsV()[h][v] = new BoundSubSolver( // Vertical Bound
                    this, h, v, false,
                    h > 0 ? subSolvers->getAreas()[h - 1][v] : 0,
                    !lastH ? subSolvers->getAreas()[h][v] : 0,
                    structAnalyzer, fdAnalyzer, boundAnalyzer
                );

        }
    }
    for (int h = 0; h <= subSolvers->sizeH(); h++)
    {
        for (int v = 0; v <= subSolvers->sizeV(); v++)
        {
            bool lastH = h == subSolvers->sizeH();
            bool lastV = v == subSolvers->sizeV();

            subSolvers->getCorners()[h][v] = new CornerSubSolver(
                this, h, v,
                v > 0   ? subSolvers->getBoundsV()[h][v - 1] : 0,   //  top
                !lastH  ? subSolvers->getBoundsH()[h][v]     : 0,   //  right
                !lastV  ? subSolvers->getBoundsV()[h][v]     : 0,   //  bottom
                h > 0   ? subSolvers->getBoundsH()[h - 1][v] : 0,   //  left
                structAnalyzer, fdAnalyzer
            );
        }
    }
    //
    //  Initialize area solvers.
    ////////////////////////////////////////////////////////////////////////////


    this->dataModel = new DataModel(this, structAnalyzer, fdAnalyzer);
    this->setTimeStep(fdAnalyzer->getTimeStep());


    ////////////////////////////////////////////////////////////////////////////
    //  Initialize transducer.
    //
    if (config->transducer().present())
    {
        if (!(transducer = factory->createTransducer(this, &config->transducer().get())))
        {
            throw Exception("A transducer type is not supported.");
        }
    }
    else
    {
        transducer = 0;
    }
    //
    //  Initialize transducer.
    ////////////////////////////////////////////////////////////////////////////


    ////////////////////////////////////////////////////////////////////////////
    //  Add listeners (output generators, stop conditions and so on).
    //
    fdAnalyzer->configureOutputs(this, this, factory);
    fdAnalyzer->configureStopConditions(this, this, factory);
    fdAnalyzer->configureTimeStepAdjusters(this, this, factory);
    //
    //  Add listeners.
    ////////////////////////////////////////////////////////////////////////////

    LOG_DEBUG(LOGGER << "Solver()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::Solver::~Solver()
{
    LOG_DEBUG(LOGGER << "~Solver()");

    if (transducer)
    {
        delete transducer;
        transducer = 0;
    }

    delete dataModel;

    for (int h = 0; h <= subSolvers->sizeH(); h++)
    {
        for (int v = 0; v <= subSolvers->sizeV(); v++)
        {
            bool lastH = h == subSolvers->sizeH();
            bool lastV = v == subSolvers->sizeV();

            if (!(lastH || lastV))
            {
                subSolvers->getAreas()[h][v]->destroy();
                delete subSolvers->getAreas()[h][v];
            }
            if (!lastH) delete subSolvers->getBoundsH()[h][v];
            if (!lastV) delete subSolvers->getBoundsV()[h][v];
            delete subSolvers->getCorners()[h][v];
        }
    }
    delete subSolvers;

    delete fdAnalyzer;
    delete boundAnalyzer;
    delete structAnalyzer;

    delete solverState;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_DM_NS::IDataModel* BIO_SLV_FD_IM2D_NS::Solver::getData()
{
    return dataModel;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::Solver::SplittedSolver* BIO_SLV_FD_IM2D_NS::Solver::getSubSolvers()
{
    return subSolvers;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::ITransducer* BIO_SLV_FD_IM2D_NS::Solver::getTransducer()
{
    return transducer;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::Solver::setState(BIO_SLV_NS::ISolverState* state)
{
    setIterationState(state->getIteration(), state->getTime(), this->getTimeStep());
    getData()->setState(state->getData());

    resetListeners();
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::ISolverState* BIO_SLV_FD_IM2D_NS::Solver::getState()
{
    return solverState;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::Solver::solveIteration()
{
    LOG_DEBUG(LOGGER << "solveIteration(" << getSolvedIterationCount() << ")...");

    ////////////////////////////////////////////////////////////////////////////
    //  Solve "horizintal" half-step in time
    //
    LOG_TRACE(LOGGER << "solveIteration: Solve horizintal half-step in time, Forward");
    for (int h = 0; h <= subSolvers->sizeH(); h++)   // Forward
    {
        for (int v = 0; v <= subSolvers->sizeV(); v++)
        {
            bool lastH = h == subSolvers->sizeH();
            bool lastV = v == subSolvers->sizeV();

            if (!lastV)
                subSolvers->getBoundsV()[h][v]->solveThroughForward();

            if (!(lastH || lastV))
                subSolvers->getAreas()[h][v]->solveHorizontalForward();

            if (!lastH)
                subSolvers->getBoundsH()[h][v]->solveAlongForward();

            subSolvers->getCorners()[h][v]->solveForward();
        }
    }
    LOG_TRACE(LOGGER << "solveIteration: Solve horizintal half-step in time, Backward");
    for (int h = subSolvers->sizeH(); h >= 0; h--)  // Backward
    {
        for (int v = 0; v <= subSolvers->sizeV(); v++)
        {
            bool lastH = h == subSolvers->sizeH();
            bool lastV = v == subSolvers->sizeV();

            if (!lastV)
                subSolvers->getBoundsV()[h][v]->solveThroughBackward();

            if (!(h == 0 || lastV))
                subSolvers->getAreas()[h - 1][v]->solveHorizontalBackward();

            if (!lastH)
                subSolvers->getBoundsH()[h][v]->solveAlongBackward();

            subSolvers->getCorners()[h][v]->solveBackward();
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    //  Solve "vertical" half-step in time
    //
    LOG_TRACE(LOGGER << "solveIteration: Solve vertical half-step in time, Forward");
    for (int v = 0; v <= subSolvers->sizeV(); v++)   // Forward
    {
        for (int h = 0; h <= subSolvers->sizeH(); h++)
        {
            bool lastH = h == subSolvers->sizeH();
            bool lastV = v == subSolvers->sizeV();

            if (!lastH)
                subSolvers->getBoundsH()[h][v]->solveThroughForward();

            if (!(lastH || lastV))
                subSolvers->getAreas()[h][v]->solveVerticalForward();

            if (!lastV)
                subSolvers->getBoundsV()[h][v]->solveAlongForward();

            subSolvers->getCorners()[h][v]->solveForward();
        }
    }
    LOG_TRACE(LOGGER << "solveIteration: Solve vertical half-step in time, Backward");
    for (int v = subSolvers->sizeV(); v >= 0; v--)  // Backward
    {
        for (int h = 0; h <= subSolvers->sizeH(); h++)
        {
            bool lastH = h == subSolvers->sizeH();
            bool lastV = v == subSolvers->sizeV();

            if (!lastH)
                subSolvers->getBoundsH()[h][v]->solveThroughBackward();

            if (!(lastH || v == 0))
                subSolvers->getAreas()[h][v - 1]->solveVerticalBackward();

            if (!lastV)
                subSolvers->getBoundsV()[h][v]->solveAlongBackward();

            subSolvers->getCorners()[h][v]->solveBackward();
        }
    }

    LOG_TRACE(LOGGER << "solveIteration()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
