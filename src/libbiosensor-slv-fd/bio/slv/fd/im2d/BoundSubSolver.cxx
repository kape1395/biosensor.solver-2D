/*
 * Copyright 2011-2013 Karolis Petrauskas
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
#include "ModelReaction.hxx"
#include "BoundSubSolver.hxx"
#include "ConstantCondition.hxx"
#include "GradCondition.hxx"
#include "WallCondition.hxx"
#include "MergeCondition.hxx"
#include "IAreaEdgeFunction.hxx"
#include "ConstantOnEdge.hxx"
#include "SubstanceConcOnEdge.hxx"
#include "SubstanceGradOnEdge.hxx"
#include "FunctionSumOnEdge.hxx"
#include <bio/cfg/ReactionAnalyzer.hxx>
#include <bio/Logging.hxx>
#include <bio/Exception.hxx>
#include <cmath>
#include <string>
#include <vector>
#include <memory>
#define LOGGER "libbiosensor-slv-fd::im2d::BoundSubSolver: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::BoundSubSolver::BoundSubSolver(
    Solver* solver,
    unsigned positionH,
    unsigned positionV,
    bool horizontal,
    AreaSubSolver* areaPrev,
    AreaSubSolver* areaNext,
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
    BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer* fdAnalyzer,
    BIO_CFG_NS::BoundAnalyzer* boundAnalyzer
)
{
    LOG_DEBUG(LOGGER << "BoundSubSolver(posH=" << positionH << ", posV=" << positionV << ", horiz=" << horizontal << ")...");

    this->solver = solver;
    this->positionH = positionH;
    this->positionV = positionV;
    this->horizontal = horizontal;
    this->structAnalyzer = structAnalyzer;
    this->fdAnalyzer = fdAnalyzer;
    this->boundAnalyzer = boundAnalyzer;
    this->segmentSplit = horizontal
                         ? fdAnalyzer->getAxisPartSegmentSplitH(positionH)
                         : fdAnalyzer->getAxisPartSegmentSplitV(positionV)
                         ;

    substanceToBCMap = new IBoundCondition*[structAnalyzer->getSubstances().size()];

    for (unsigned s = 0; s < structAnalyzer->getSubstances().size(); s++)
    {
        substanceToBCMap[s] = 0;

        if (horizontal)
        {
            if (positionV > 0)
            {
                createBoundCondition(
                    boundAnalyzer->getBoundForSubstance(s, positionH, positionV - 1, boundAnalyzer->BOTTOM),
                    boundAnalyzer->getRelatedReactions(s, positionH, positionV - 1, boundAnalyzer->BOTTOM),
                    areaPrev, areaNext, s, false
                );
            }
            if (positionV < structAnalyzer->getPointsV().size() - 1)
            {
                createBoundCondition(
                    boundAnalyzer->getBoundForSubstance(s, positionH, positionV, boundAnalyzer->TOP),
                    boundAnalyzer->getRelatedReactions(s, positionH, positionV, boundAnalyzer->TOP),
                    areaPrev, areaNext, s, true
                );
            }
        }
        else // vertical
        {
            if (positionH > 0)
            {
                createBoundCondition(
                    boundAnalyzer->getBoundForSubstance(s, positionH - 1, positionV, boundAnalyzer->RIGHT),
                    boundAnalyzer->getRelatedReactions(s, positionH - 1, positionV, boundAnalyzer->RIGHT),
                    areaPrev, areaNext, s, false
                );
            }
            if (positionH < structAnalyzer->getPointsH().size() - 1)
            {
                createBoundCondition(
                    boundAnalyzer->getBoundForSubstance(s, positionH, positionV, boundAnalyzer->LEFT),
                    boundAnalyzer->getRelatedReactions(s, positionH, positionV, boundAnalyzer->LEFT),
                    areaPrev, areaNext, s, true
                );
            }
        }
    }

    LOG_DEBUG(LOGGER << "BoundSubSolver(posH=" << positionH << ", posV=" << positionV << ", horiz=" << horizontal << ")... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::BoundSubSolver::~BoundSubSolver()
{
    LOG_TRACE(LOGGER << "~BoundSubSolver()...");

    for (BCIterator bc = boundConditions.begin(); bc < boundConditions.end(); bc++)
    {
        delete *bc;
    }
    boundConditions.clear();

    for (std::vector<IAreaEdgeData*>::iterator i = allocatedEdges.begin(); i != allocatedEdges.end(); i++)
    {
        delete *i;
    }
    allocatedEdges.clear();

    for (std::vector<IAreaEdgeFunction*>::iterator i = allocatedFunctions.begin(); i != allocatedFunctions.end(); i++)
    {
        delete *i;
    }
    allocatedFunctions.clear();

    delete [] substanceToBCMap;

    LOG_TRACE(LOGGER << "~BoundSubSolver()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::BoundSubSolver::solveThroughForward()
{
    LOG_TRACE(LOGGER << "solveThroughForward()...");
    for (BCIterator bc = boundConditions.begin(); bc < boundConditions.end(); bc++)
    {
        (*bc)->solveThroughForward();
    }
    LOG_TRACE(LOGGER << "solveThroughForward()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::BoundSubSolver::solveThroughBackward()
{
    LOG_TRACE(LOGGER << "solveThroughBackward()...");
    for (BCIterator bc = boundConditions.begin(); bc < boundConditions.end(); bc++)
    {
        (*bc)->solveThroughBackward();
    }
    LOG_TRACE(LOGGER << "solveThroughBackward()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::BoundSubSolver::solveAlongForward()
{
    LOG_TRACE(LOGGER << "solveAlongForward()...");
    for (BCIterator bc = boundConditions.begin(); bc < boundConditions.end(); bc++)
    {
        (*bc)->solveAlongForward();
    }
    LOG_TRACE(LOGGER << "solveAlongForward()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::BoundSubSolver::solveAlongBackward()
{
    LOG_TRACE(LOGGER << "solveAlongBackward()...");
    for (BCIterator bc = boundConditions.begin(); bc < boundConditions.end(); bc++)
    {
        (*bc)->solveAlongBackward();
    }
    LOG_TRACE(LOGGER << "solveAlongBackward()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::BoundSubSolver::applyInitialValues()
{
    LOG_DEBUG(LOGGER << "applyInitialValues()...");
    for (BCIterator bc = boundConditions.begin(); bc < boundConditions.end(); bc++)
    {
        (*bc)->applyInitialValues();
    }
    LOG_DEBUG(LOGGER << "applyInitialValues()... Done");
}

/* ************************************************************************** */
/* ************************************************************************** */
BIO_XML_MODEL_NS::reaction::ReductionOxidation *
BIO_SLV_FD_IM2D_NS::BoundSubSolver::get_single_ro_bound_reaction(
    const std::vector<BIO_XML_MODEL_NS::Reaction*>& boundReactions
)
{
    if (boundReactions.size() != 1)
        throw Exception("Only single bound reaction (related to one substance) is supported for now...");
    BIO_XML_MODEL_NS::Reaction *reaction = boundReactions[0];

    BIO_XML_MODEL_NS::reaction::ReductionOxidation *ro;
    ro = dynamic_cast<BIO_XML_MODEL_NS::reaction::ReductionOxidation*>(reaction);
    if (!ro)
        throw Exception("Only ReductionOxidation reaction is supported on bound.");

    if (ro->substrate().size() != 1)
        throw Exception("Only reaction with one substrate is supported for now.");
    return ro;
}

BIO_XML_MODEL_NS::reaction::ReductionOxidation *
BIO_SLV_FD_IM2D_NS::BoundSubSolver::get_single_ro_fast_bound_reaction(
    const std::vector<BIO_XML_MODEL_NS::Reaction*>& boundReactions
)
{
    BIO_XML_MODEL_NS::reaction::ReductionOxidation *ro = get_single_ro_bound_reaction(boundReactions);

    if (!is_ro_reaction_fast(ro))
        throw Exception("Only reaction with infinite rate is supported for now.");
    return ro;
}

bool
BIO_SLV_FD_IM2D_NS::BoundSubSolver::is_ro_reaction_fast(
    BIO_XML_MODEL_NS::reaction::ReductionOxidation *ro
)
{
    return std::isinf(structAnalyzer->getSymbol(ro->rate())->value());
}


void BIO_SLV_FD_IM2D_NS::BoundSubSolver::createBoundCondition(
    BIO_XML_MODEL_NS::BoundSubstance * boundSubstance,
    const std::vector<BIO_XML_MODEL_NS::Reaction*>& boundReactions,
    AreaSubSolver* areaPrev,
    AreaSubSolver* areaNext,
    int substance,
    bool atStart
)
{
    LOG_DEBUG(LOGGER << "createBoundCondition(substance=" << substance << " atStart=" << atStart << ")...");

    IBoundCondition *bc = 0;
    if (dynamic_cast<BIO_XML_MODEL_NS::bound::Constant*>(boundSubstance) != 0)
    {
        LOG_DEBUG(LOGGER << "createBoundCondition: type=Constant");
        if (boundReactions.size() > 0)
        {
            LOG_WARN(LOGGER << "Ignoring reactions, that are related to CONST bound condition");
            //  FIXME:  Implement reactions on bounds properly.
        }
        BIO_XML_MODEL_NS::bound::Constant* bsConst = dynamic_cast<BIO_XML_MODEL_NS::bound::Constant*>(boundSubstance);
        bc = new ConstantCondition(
            (atStart ? areaNext : areaPrev)->getEdgeData(substance, horizontal, atStart),
            structAnalyzer->getSymbol(bsConst->concentration())->value(),
            atStart
        );
    }
    else if (dynamic_cast<BIO_XML_MODEL_NS::bound::Wall*>(boundSubstance) != 0)
    {
        LOG_DEBUG(LOGGER << "createBoundCondition: type=Wall");
        AreaSubSolver* area = (atStart ? areaNext : areaPrev);
        if (boundReactions.size() > 0)
        {
            LOG_DEBUG(LOGGER << "createBoundCondition: reactions exists, analyzing...");
            BIO_XML_MODEL_NS::reaction::ReductionOxidation *ro = get_single_ro_bound_reaction(boundReactions);
            std::auto_ptr<BIO_CFG_NS::ReactionAnalyzer> roa(BIO_CFG_NS::ReactionAnalyzer::newAnalyzer(ro));

            if (roa->isProduct(structAnalyzer->getSubstances()[substance]->name()))
            {
                //  Gradient should be applied to the product of the reaction.
                LOG_DEBUG(LOGGER << "createBoundCondition: substance is in products of the reaction, function will be constructed...");

                int roS = structAnalyzer->getSubstanceIndex(ro->substrate()[0].name());
                int roP = substance;
                //  NOTE: This implementation implies requirements on rhe ordering of substances.
                //        The substrate must be listed before the product.
                if (roS >= roP)
                    throw Exception("Wrong order of substances.");

                double diffS = structAnalyzer->getDiffusionCoef(
                    roS,
                    area->getPositionH(), area->getPositionV(), !horizontal
                );
                double diffP = structAnalyzer->getDiffusionCoef(
                    roP,
                    area->getPositionH(), area->getPositionV(), !horizontal
                );
                IAreaEdgeFunction *function = new SubstanceGradOnEdge(
                    area->getEdgeData(roS, horizontal, atStart),
                     -diffS // Multiplier -1 is used here to model generation of the material.
                );
                bc = new GradCondition(
                    area->getEdgeData(roP, horizontal, atStart),
                    atStart,
                    diffP,
                    function
                );
                allocatedFunctions.push_back(function);
            }
            else if (roa->isSubstrate(structAnalyzer->getSubstances()[substance]->name()))
            {

                //  Zero concentration should be applied to the substrate of the reaction.
                //  NOTE: This is currently has no meaning, because the user must specify the zero-concentration
                //        condition explicitly because of the way, how BoundAnalyser is implemented (bounds
                //        are tied to geometry AND SUBSTANCES).
                //  NOTE: It has a meaning, if user specifies wall condition instead of zero concentration.
                if (is_ro_reaction_fast(ro))
                {
                    bc = new ConstantCondition(
                        area->getEdgeData(substance, horizontal, atStart),
                        0.0,
                        atStart
                    );
                }
                else if (!ro->bw_rate().present())
                {
                    //
                    //  Reaction has finite rate but is not reversible.
                    //
                    IAreaEdgeFunction *function = new SubstanceConcOnEdge(
                        area->getEdgeData(substance, horizontal, atStart),
                        structAnalyzer->getSymbol(ro->rate())->value()
                    );
                    double diff = structAnalyzer->getDiffusionCoef(
                        substance,
                        area->getPositionH(), area->getPositionV(), !horizontal
                    );
                    bc = new GradCondition(
                        area->getEdgeData(substance, horizontal, atStart),
                        atStart,
                        diff,
                        function
                    );
                    allocatedFunctions.push_back(function);
                }
                else
                {
                    //
                    //  Reaction has finite rates and is reversible.
                    //
                    int roS = substance;
                    int roP = structAnalyzer->getSubstanceIndex(ro->product()[0].name());
                    IAreaEdgeFunction *function_fw = new SubstanceConcOnEdge(
                        area->getEdgeData(roS, horizontal, atStart),
                        structAnalyzer->getSymbol(ro->rate())->value()
                    );
                    IAreaEdgeFunction *function_bw = new SubstanceConcOnEdge(
                        area->getEdgeData(roP, horizontal, atStart),
                        -structAnalyzer->getSymbol(ro->bw_rate().get())->value()
                    );
                    IAreaEdgeFunction *function = new FunctionSumOnEdge(
                        function_fw,
                        function_bw,
                        1.0
                    );
                    double diff = structAnalyzer->getDiffusionCoef(
                        roS,
                        area->getPositionH(), area->getPositionV(), !horizontal
                    );
                    bc = new GradCondition(
                        area->getEdgeData(roS, horizontal, atStart),
                        atStart,
                        diff,
                        function
                    );
                    allocatedFunctions.push_back(function_fw);
                    allocatedFunctions.push_back(function_bw);
                    allocatedFunctions.push_back(function);
                }
            }

            LOG_DEBUG(LOGGER << "createBoundCondition: reactions exists, analyzing... Done");
        } // if (boundReactions.size() > 0)
        else
        {
            bc = new WallCondition(
                area->getEdgeData(substance, horizontal, atStart),
                atStart
            );
        }
    }
    else if (dynamic_cast<BIO_XML_MODEL_NS::bound::Merge*>(boundSubstance) != 0)
    {
        if (atStart)    // This BC is for both sides so only one should be created.
        {
            LOG_DEBUG(LOGGER << "createBoundCondition: type=Merge");
            IAreaEdgeFunction *function = 0;
            if  (boundReactions.size() != 0)
            {
                LOG_DEBUG(LOGGER << "createBoundCondition: reactions exists, analyzing...");
                BIO_XML_MODEL_NS::reaction::ReductionOxidation *ro = get_single_ro_fast_bound_reaction(boundReactions);
                std::auto_ptr<BIO_CFG_NS::ReactionAnalyzer> roa(BIO_CFG_NS::ReactionAnalyzer::newAnalyzer(ro));

                //  Function must be applied only when base substance is a product.
                if (roa->isProduct(structAnalyzer->getSubstances()[substance]->name()))
                {
                    LOG_DEBUG(LOGGER << "createBoundCondition: substance is in products of the reaction, function will be constructed...");

                    //  dabar mums reikia nuspresti, is kokios srities imti ta edge,
                    //
                    int roS = structAnalyzer->getSubstanceIndex(ro->substrate()[0].name());
                    if ((areaPrev->getSubstanceConf(roS) == 0 && areaNext->getSubstanceConf(roS) == 0) ||
                            (areaPrev->getSubstanceConf(roS) != 0 && areaNext->getSubstanceConf(roS) != 0))
                        throw Exception("Substrate of the reaction should be found only in one of the areas.");

                    if (areaPrev->getSubstanceConf(roS) != 0)
                    {
                        LOG_DEBUG(LOGGER << "createBoundCondition: Creating SubstanceGradOnEdge for end@areaPrev, substrate=" << roS);
                        function = new SubstanceGradOnEdge(
                            areaPrev->getEdgeData(roS, horizontal, false, false),   // at end, previous layer
                            structAnalyzer->getDiffusionCoef(roS, areaPrev->getPositionH(), areaPrev->getPositionV(), !horizontal)
                        );
                    }
                    else
                    {
                        LOG_DEBUG(LOGGER << "createBoundCondition: Creating SubstanceGradOnEdge for start@areaNext, substrate=" << roS);
                        function = new SubstanceGradOnEdge(
                            areaNext->getEdgeData(roS, horizontal, true, false),    // at start, previous layer
                            structAnalyzer->getDiffusionCoef(roS, areaNext->getPositionH(), areaNext->getPositionV(), !horizontal)
                        );
                    }
                    LOG_DEBUG(LOGGER << "createBoundCondition: substance is in products of the reaction, function will be constructed... Done");
                }
                LOG_DEBUG(LOGGER << "createBoundCondition: reactions exists, analyzing... Done");
            }
            if (!function)
            {
                function = new ConstantOnEdge(getPointPositions()->getPointCount(), 0.0);
            }
            allocatedFunctions.push_back(function);


            bc = new MergeCondition(
                areaPrev->getEdgeData(substance, horizontal, false),    // atEnd
                areaNext->getEdgeData(substance, horizontal, true),     // atStart
                structAnalyzer->getDiffusionCoef(substance, areaPrev->getPositionH(), areaPrev->getPositionV(), !horizontal),
                structAnalyzer->getDiffusionCoef(substance, areaNext->getPositionH(), areaNext->getPositionV(), !horizontal),
                function
            );
        }
        else
        {
            LOG_DEBUG(LOGGER << "createBoundCondition: type=Merge (ignoring, because it is not at start of the area)");
        }
    }
    else if (dynamic_cast<BIO_XML_MODEL_NS::bound::Null*>(boundSubstance) != 0)
    {
        LOG_DEBUG(LOGGER << "createBoundCondition: type=Null");
        // nothing
        bc = 0;
    }
    else
    {
        throw BIO_NS::Exception("Unknown bound condition");
    }

    if (bc != 0)
    {
        boundConditions.push_back(bc);

        if (substanceToBCMap[substance] == 0)
        {
            substanceToBCMap[substance] = bc;
        }
        else
        {
            throw Exception("Two bound conditions on one edge for one substance is not supported.");
        }
    }
    LOG_DEBUG(LOGGER << "createBoundCondition(substance=" << substance << " atStart=" << atStart << ")... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
int BIO_SLV_FD_IM2D_NS::BoundSubSolver::getSubstanceCount()
{
    return structAnalyzer->getSubstances().size();
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_XML_MODEL_NS::Substance* BIO_SLV_FD_IM2D_NS::BoundSubSolver::getSubstanceConf(int index)
{
    return (substanceToBCMap[index])
           ? structAnalyzer->getSubstances()[index]
           : 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_DM_NS::ISegmentSplit* BIO_SLV_FD_IM2D_NS::BoundSubSolver::getPointPositions()
{
    return segmentSplit;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_DM_NS::ICursor1D* BIO_SLV_FD_IM2D_NS::BoundSubSolver::newGridCursor()
{
    return new Cursor(this);
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_SLV_FD_IM2D_NS::BoundSubSolver::getConcentration(int x, int s)
{
    return (substanceToBCMap[s])
           ? substanceToBCMap[s]->getConcentration(x)
           : NAN;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::BoundSubSolver::setConcentration(int x, int s, double c)
{
    if (substanceToBCMap[s] != 0)
    {
        substanceToBCMap[s]->setConcentration(x, c);
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::BoundSubSolver::Cursor::Cursor(BoundSubSolver* subSolver)
{
    this->subSolver = subSolver;
    this->position = 0;
    this->pointCount = subSolver->getPointPositions()->getPointCount();
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::BoundSubSolver::Cursor::~Cursor()
{
    // Nothing to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::BoundSubSolver::Cursor::prev()
{
    position--;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::BoundSubSolver::Cursor::next()
{
    position++;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::BoundSubSolver::Cursor::start()
{
    position = 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::BoundSubSolver::Cursor::end()
{
    position = pointCount - 1;
}


/* ************************************************************************** */
/* ************************************************************************** */
bool BIO_SLV_FD_IM2D_NS::BoundSubSolver::Cursor::isValid()
{
    return position >= 0 && position < pointCount;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_DM_NS::IConcentrations* BIO_SLV_FD_IM2D_NS::BoundSubSolver::Cursor::getConcentrations()
{
    if (!isValid())
    {
        return 0;
    }
    return this;
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_SLV_FD_IM2D_NS::BoundSubSolver::Cursor::getConcentration(int substanceNr)
{
    return subSolver->getConcentration(position, substanceNr);
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::BoundSubSolver::Cursor::setConcentration(int substanceNr, double concentration)
{
    subSolver->setConcentration(position, substanceNr, concentration);
}


/* ************************************************************************** */
/* ************************************************************************** */
