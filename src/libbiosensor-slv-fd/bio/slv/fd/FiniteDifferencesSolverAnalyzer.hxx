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
#ifndef BIO_SLV_FD_FiniteDifferencesSolverAnalyzer_HXX
#define BIO_SLV_FD_FiniteDifferencesSolverAnalyzer_HXX
#include "../../../biosensor-slv-fd.hxx"
#include <biosensor-xml.hxx>
#include <bio/IFactory.hxx>
#include <bio/slv/IIterativeSolver.hxx>
#include <bio/cfg/StructureAnalyzer.hxx>
#include <bio/dm/ISegmentSplit.hxx>
#include <vector>

BIO_SLV_FD_NS_BEGIN


/**
 *
 */
class FiniteDifferencesSolverAnalyzer
{
private:

    BIO_XML_NS::model::Model* config;

    int partCountH;
    int partCountV;

    BIO_CFG_NS::StructureAnalyzer structureAnalyzer;
    BIO_XML_NS::model::solver::Axis** axisPartsH;
    BIO_XML_NS::model::solver::Axis** axisPartsV;
    BIO_DM_NS::ISegmentSplit** axisPartSegmentSplitH;
    BIO_DM_NS::ISegmentSplit** axisPartSegmentSplitV;

    BIO_XML_NS::model::solver::Axis* pseudoAxisH;

    double timeStep;

public:

    /**
     *  Constructor.
     *  \param config  Configuration to be analyzed.
     */
    FiniteDifferencesSolverAnalyzer(BIO_XML_NS::model::Model* config);

    /**
     *  Destructor.
     */
    virtual ~FiniteDifferencesSolverAnalyzer();

    /**
     *  \deprecated { Use #getAxisPartSegmentSplitH instead. }
     */
    BIO_XML_NS::model::solver::Axis* getAxisPartH(int index)
    {
        return axisPartsH[index];
    }

    /**
     *  \deprecated { Use #getAxisPartSegmentSplitV instead. }
     */
    BIO_XML_NS::model::solver::Axis* getAxisPartV(int index)
    {
        return axisPartsV[index];
    }

    /**
     *
     */
    BIO_DM_NS::ISegmentSplit* getAxisPartSegmentSplitH(int index)
    {
        return axisPartSegmentSplitH[index];
    }

    /**
     *
     */
    BIO_DM_NS::ISegmentSplit* getAxisPartSegmentSplitV(int index)
    {
        return axisPartSegmentSplitV[index];
    }

    /**
     *  Returns times step, defined in the solver tag.
     */
    double getTimeStep()
    {
        return timeStep;
    }

    /**
     *
     */
    BIO_XML_MODEL_NS::solver::FiniteDifferences* getFDSolverConfig();

    /**
     *  Returns output configurations.
     *  TODO: Move this to a more generic solver config analyzer.
     */
    void configureOutputs(
        BIO_SLV_NS::ISolver*            solver,
        BIO_SLV_NS::IIterativeSolver*   iterativeSolver,
        BIO_NS::IFactory*               factory
    );

    /**
     *
     */
    void configureStopConditions(
        BIO_SLV_NS::ISolver*            solver,
        BIO_SLV_NS::IIterativeSolver*   iterativeSolver,
        BIO_NS::IFactory*               factory
    );

    /**
     *
     */
    void configureTimeStepAdjusters(
        BIO_SLV_NS::ISolver*            solver,
        BIO_SLV_NS::IIterativeSolver*   iterativeSolver,
        BIO_NS::IFactory*               factory
    );

protected:
    BIO_DM_NS::ISegmentSplit* createSegmentSplit(BIO_XML_NS::model::solver::Axis* axis);


};


BIO_SLV_FD_NS_END
#endif
