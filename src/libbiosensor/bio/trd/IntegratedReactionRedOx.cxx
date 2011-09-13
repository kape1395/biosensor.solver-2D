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
#include "IntegratedReaction.hxx"
#include "../Exception.hxx"
#include "../Logging.hxx"
#include "IntegratedReactionRedOx.hxx"
#include <vector>
#include <algorithm>
#define LOGGER "libbiosensor::IntegratedReactionRedOx: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::IntegratedReactionRedOx::IntegratedReactionRedOx(
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
    BIO_XML_MODEL_NS::ReactionName& reactionName
) : IntegratedReaction(structAnalyzer, reactionName)
{
    using namespace BIO_XML_MODEL_NS::reaction;

    ReductionOxidation* ro = dynamic_cast<ReductionOxidation*>(reaction);

    if (!ro)
        throw Exception("Only integral for ReductionOxidation is supported by this class");

    rate = structAnalyzer->getSymbol(ro->rate())->value();

    LOG_DEBUG(LOGGER << "Reaction rate=" << rate);
    LOG_DEBUG(LOGGER << "Collection substances to use in calculation...");
    for (ReductionOxidation::substrate_iterator s = ro->substrate().begin(); s != ro->substrate().end(); s++)
    {
        substrates.push_back(structAnalyzer->getSubstanceIndex(s->name()));
        LOG_DEBUG(LOGGER << "Added substance: " << s->name());
    }
    LOG_DEBUG(LOGGER << "Collection substances to use in calculation... Done");

    if (substrates.size() < 1)
        throw Exception("At least 1 substrate must exist in the RedOx reaction");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::IntegratedReactionRedOx::~IntegratedReactionRedOx()
{
    //  Nothiong to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_TRD_NS::IntegratedReactionRedOx::getValue(BIO_DM_NS::IConcentrations* c)
{
    double value = rate;
    LOG_TRACE(LOGGER << "Calculate value...");
    for (std::vector<int>::iterator s = substrates.begin(); s != substrates.end(); s++)
    {
        LOG_TRACE(LOGGER << "Multiplying by: " << c->getConcentration(*s));
        value *= c->getConcentration(*s);
    }
    LOG_TRACE(LOGGER << "Calculate value... done, value=" << value);
    return value;
}


/* ************************************************************************** */
/* ************************************************************************** */
