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
#include "IntegratedReactionRedOx.hxx"
#include <vector>
#include <algorithm>


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::IntegratedReaction::IntegratedReaction(
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
    BIO_XML_MODEL_NS::ReactionName& reactionName
) : IIntegratedExpression()
{
    this->structAnalyzer = structAnalyzer;
    this->reaction = structAnalyzer->getReaction(reactionName);
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::IntegratedReaction::~IntegratedReaction()
{
    //  Nothiong to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
bool BIO_TRD_NS::IntegratedReaction::isDefined(int h, int v)
{
    typedef std::vector<BIO_XML_MODEL_NS::Reaction*> v_reac;
    v_reac rv = structAnalyzer->getReactions(h, v);
    for (v_reac::iterator r = rv.begin(); r < rv.end(); r++)
    {
        if ((*r)->name().compare(reaction->name()) == 0)
        {
            return true;
        }
    }
    return false;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::IntegratedReaction* BIO_TRD_NS::IntegratedReaction::newInstance(
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
    BIO_XML_MODEL_NS::ReactionName& reactionName
)
{
    using BIO_XML_MODEL_NS::Reaction;
    using namespace BIO_XML_MODEL_NS::reaction;
    Reaction* reaction = structAnalyzer->getReaction(reactionName);

    if (dynamic_cast<ReductionOxidation*>(reaction))
    {
        return new IntegratedReactionRedOx(structAnalyzer, reactionName);
    }
    else
    {
        throw Exception("Integral of specified reaction is not supported for now...");
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
