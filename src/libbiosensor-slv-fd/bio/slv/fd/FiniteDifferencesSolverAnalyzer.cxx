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
#include "FiniteDifferencesSolverAnalyzer.hxx"
#include "ModelSolver.hxx"
#include "bio/slv/IIterativeSolver.hxx"
#include <bio/Logging.hxx>
#include <bio/Exception.hxx>
#include <bio/dm/ConstantSegmentSplit.hxx>
#include <bio/slv/ISolverListener.hxx>
#include <vector>
#define LOGGER "libbiosensor-slv-fd::FiniteDifferencesSolverAnalyzer: "


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  NOTE: There is a litle of code duplication.
 *  NOTE: For now only ConstantAxisPart elements are supported.
 */
BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer::FiniteDifferencesSolverAnalyzer(
    BIO_XML_NS::model::Model*      config,
    BIO_CFG_NS::StructureAnalyzer* structureAnalyzer,
    BIO_CFG_NS::ISymbolResolver*   symbolResolver
)
{
    this->config = 0;
    this->axisPartsH = 0;
    this->axisPartsV = 0;

    using BIO_XML_NS::model::Symbol;
    using BIO_XML_NS::model::solver::FiniteDifferences;
    using BIO_XML_NS::model::solver::Axis;

    typedef std::vector<Symbol*>::iterator Symbol_it;
    typedef FiniteDifferences::axis_iterator Axis_it;

    LOG_DEBUG(LOGGER << "FiniteDifferencesSolverAnalyzer()...");

    this->config = config;
    this->structureAnalyzer = structureAnalyzer;
    this->symbolResolver = symbolResolver;

    FiniteDifferences* fdSolver = dynamic_cast<FiniteDifferences*>(&config->solver());
    if (!fdSolver)
    {
        LOG_ERROR(LOGGER << "Received configuration with non FiniteDifferences solver. I dont know what to do with it.");
        throw Exception("Invalid solver spec.");
    }

    //  Create pseudo axis for 1d case
    pseudoAxisH = new BIO_XML_MODEL_NS::solver::ConstantAxisPart(
        structureAnalyzer->getPointsH()[0]->name(),
        structureAnalyzer->getPointsH()[1]->name(),
        "2"   // step count, implicit symbol declaration
    );


    ////////////////////////////////////////////////////////////////////////////
    //  Collect axis-parts and assign them to the concrete "cells"
    //
    partCountH = structureAnalyzer->getPointsH().size() - 1; // intervalu yra 1 maziau nei tasku.
    partCountV = structureAnalyzer->getPointsV().size() - 1; // intervalu yra 1 maziau nei tasku.

    axisPartsH = new Axis*[partCountH];
    axisPartsV = new Axis*[partCountV];
    axisPartSegmentSplitH = new BIO_DM_NS::ISegmentSplit*[partCountH];
    axisPartSegmentSplitV = new BIO_DM_NS::ISegmentSplit*[partCountV];

    for (int i = 0; i < partCountH; i++)
    {
        axisPartsH[i] = 0;
        axisPartSegmentSplitH[i] = 0;
    }
    for (int i = 0; i < partCountV; i++)
    {
        axisPartsV[i] = 0;
        axisPartSegmentSplitV[i] = 0;
    }

    std::vector<Symbol*>* symbols;


    ////////////    Analyze HORIZONTAL axis
    if (structureAnalyzer->isOneDimensional())
    {
        axisPartsH[0] = pseudoAxisH;
        axisPartSegmentSplitH[0] = this->createSegmentSplit(pseudoAxisH);
    }
    else
    {
        symbols = &(structureAnalyzer->getPointsH());
        int pointPosition = 0;
        for (Symbol_it point = symbols->begin(); point < symbols->end() - 1; point++, pointPosition++)
        {
            for (Axis_it axis = fdSolver->axis().begin(); axis < fdSolver->axis().end(); axis++)
            {
                if (axis->from() != (*point)->name())
                    continue;   //  If this is not a needed axis - skip it.

                if (axis->to() != (*(point+1))->name())
                {
                    LOG_ERROR(LOGGER << "In solver/axis \"from\" and \"to\" must be subsequent points in the corresponding axis.");
                    throw Exception("Invalid solver spec.");
                }

                axisPartsH[pointPosition] = &*axis;
                axisPartSegmentSplitH[pointPosition] = this->createSegmentSplit(&*axis);
            }
        }
    }


    ////////////    Analyze VERTICAL axis
    symbols = &(structureAnalyzer->getPointsV());
    int pointPosition = 0;
    for (Symbol_it point = symbols->begin(); point < symbols->end() - 1; point++, pointPosition++)
    {
        for (Axis_it axis = fdSolver->axis().begin(); axis < fdSolver->axis().end(); axis++)
        {
            if (axis->from() != (*point)->name())
                continue;   //  If this is not a needed axis - skip it.

            if (axis->to() != (*(point+1))->name())
            {
                LOG_ERROR(LOGGER << "In solver/axis \"from\" and \"to\" must be subsequent points in the corresponding axis.");
                throw Exception("Invalid solver spec.");
            }

            axisPartsV[pointPosition] = &*axis;
            axisPartSegmentSplitV[pointPosition] = this->createSegmentSplit(&*axis);
        }
    }

    ////////////    Check if all intervals were covered in the config.
    int unspecifiedIntervals = 0;
    for (int i = 0; i < partCountH; i++)
    {
        if (axisPartsH[i] == 0)
            unspecifiedIntervals++;
    }
    for (int i = 0; i < partCountV; i++)
    {
        if (axisPartsV[i] == 0)
            unspecifiedIntervals++;
    }
    if (unspecifiedIntervals)
    {
        LOG_ERROR(LOGGER << "Not all axis parts are specified by solver/axis elements");
        throw Exception("Invalid solver spec.");
    }

    //
    //  Collect axis-parts and assign them to the concrete "cells"
    ////////////////////////////////////////////////////////////////////////////

    this->timeStep = symbolResolver->getValueAsDouble(fdSolver->timeStep());

    LOG_DEBUG(LOGGER << "FiniteDifferencesSolverAnalyzer()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer::~FiniteDifferencesSolverAnalyzer()
{
    LOG_TRACE(LOGGER << "~FiniteDifferencesSolverAnalyzer()...");

    ////////////////////////////////////////////////////////////////////////////
    //  Release all old data.
    //
    if (axisPartsH)
    {
        delete[] axisPartsH;
        axisPartsH = 0;
    }
    if (axisPartsV)
    {
        delete[] axisPartsV;
        axisPartsV = 0;
    }

    for (int i = 0; i < partCountH; i++)
    {
        if (axisPartSegmentSplitH[i])
            delete axisPartSegmentSplitH[i];
    }
    delete [] axisPartSegmentSplitH;
    axisPartSegmentSplitH = 0;

    for (int i = 0; i < partCountV; i++)
    {
        if (axisPartSegmentSplitV[i])
            delete axisPartSegmentSplitV[i];
    }
    delete [] axisPartSegmentSplitV;
    axisPartSegmentSplitV = 0;

    delete pseudoAxisH;

    //
    //  Release all old data.
    ////////////////////////////////////////////////////////////////////////////

    LOG_TRACE(LOGGER << "~FiniteDifferencesSolverAnalyzer()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_XML_MODEL_NS::solver::FiniteDifferences* BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer::getFDSolverConfig()
{
    return dynamic_cast<BIO_XML_MODEL_NS::solver::FiniteDifferences*>(&config->solver());
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer::configureOutputs(
    BIO_SLV_NS::ISolver*          solver,
    BIO_SLV_NS::IIterativeSolver* iterativeSolver,
    BIO_NS::IFactory* factory
)
{
    BIO_XML_MODEL_NS::solver::FiniteDifferences* fd = getFDSolverConfig();

    for (BIO_XML_MODEL_NS::Solver::output_iterator out = fd->output().begin(); out < fd->output().end(); out++)
    {
        BIO_SLV_NS::ISolverListener* listener = factory->createOutput(solver, &*out);
        if (!listener)
            throw Exception("Unsupported output configuration.");

        iterativeSolver->addListener(listener, true);
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer::configureStopConditions(
    BIO_SLV_NS::ISolver*          solver,
    BIO_SLV_NS::IIterativeSolver* iterativeSolver,
    BIO_NS::IFactory*             factory
)
{
    BIO_XML_MODEL_NS::solver::FiniteDifferences* fd = getFDSolverConfig();

    for (BIO_XML_MODEL_NS::solver::FiniteDifferences::stopCondition_iterator sc = fd->stopCondition().begin();
            sc < fd->stopCondition().end(); sc++)
    {
        BIO_SLV_NS::ISolverListener* listener = factory->createStopCondition(solver, &*sc);
        if (!listener)
            throw Exception("Unsupported stopCondition configuration.");

        iterativeSolver->addListener(listener, true);
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer::configureTimeStepAdjusters(
    BIO_SLV_NS::ISolver*          solver,
    BIO_SLV_NS::IIterativeSolver* iterativeSolver,
    BIO_NS::IFactory*             factory
)
{
    BIO_XML_MODEL_NS::solver::FiniteDifferences* fd = getFDSolverConfig();

    if (fd->timeStepAdjuster().present())
    {
        BIO_SLV_NS::ISolverListener* listener = factory->createTimeStepAdjuster(solver, &(fd->timeStepAdjuster().get()));
        if (!listener)
            throw Exception("Unsupported timeStepAdjuster configuration.");

        iterativeSolver->addListener(listener, true);
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_DM_NS::ISegmentSplit* BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer::createSegmentSplit(
    BIO_XML_NS::model::solver::Axis* axis
)
{
    if (axis == 0)
        return 0;

    if (dynamic_cast<BIO_XML_NS::model::solver::ConstantAxisPart*>(axis) != 0)
    {
        BIO_XML_NS::model::solver::ConstantAxisPart* cap;
        cap = dynamic_cast<BIO_XML_NS::model::solver::ConstantAxisPart*>(axis);

        double from = symbolResolver->getValueAsDouble(axis->from());
        double to = symbolResolver->getValueAsDouble(axis->to());
        int count = (int)symbolResolver->getValueAsLong(cap->stepCount());

        BIO_DM_NS::ConstantSegmentSplit* split = new BIO_DM_NS::ConstantSegmentSplit(
            from,
            to - from,
            count
        );
        return split;
    }
    else
    {
        throw Exception("Used subclass of bio::model::solver::Axis is not supported yet.");
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
