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
#ifndef BIO_SLV_StopIfSumOfConcentrationsNonConst_HXX
#define BIO_SLV_StopIfSumOfConcentrationsNonConst_HXX
#include "../../biosensor.hxx"
#include "ISolver.hxx"
#include "ISolverListener.hxx"
#include "../dm/IComposite2D.hxx"
#include <biosensor-xml.hxx>
#include <string>
#include <vector>

BIO_SLV_NS_BEGIN


/**
 *  Stop condition.
 */
class StopIfSumOfConcentrationsNonConst : public ISolverListener
{
private:

    BIO_SLV_NS::IIterativeSolver* iterativeSolver;
    BIO_XML_MODEL_NS::MediumName mediumName;
    double constant;
    double error;
    std::vector<int> substanceIncexes;
    std::vector<BIO_DM_NS::IGrid2D*> areas;

    long checkEveryNumberOfSteps;
    long nextStepForCheck;

public:

    /**
     *  Constructor.
     *
     *  \param solver       Target solver.
     *  \param mediumName   Medium name, for which this stop condition is applied.
     *  \param constant     Constant, to which sum of the substances must be equal.
     *  \param error        Allowed error (relatime error).
     *  \param substances   Substances, concentrations of which is to be summarized.
     *  \param checkEveryNumberOfSteps  Number of steps, ...
     */
    StopIfSumOfConcentrationsNonConst(
        BIO_SLV_NS::ISolver*            solver,
        BIO_XML_MODEL_NS::MediumName&   mediumName,
        BIO_XML_MODEL_NS::SymbolName&   constant,
        double                          error,
        std::vector<BIO_XML_MODEL_NS::SubstanceName*> substances,
        long checkEveryNumberOfSteps = 100
    );

    /**
     *  Destructor.
     */
    virtual ~StopIfSumOfConcentrationsNonConst();

    /**
     *  EventListener.
     */
    virtual void solveEventOccured();

    /**
     *  Reset listener's internal state.
     */
    virtual void reset();

private:

    /**
     *  Check one sub-area.
     */
    bool checkSubArea(BIO_DM_NS::IGrid2D* area);

};



BIO_SLV_NS_END
#endif
