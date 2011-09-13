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
#include "IntegratedConcentration.hxx"
#include <vector>
#include <algorithm>


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::IntegratedConcentration::IntegratedConcentration(
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
    BIO_XML_MODEL_NS::SubstanceName& substanceName
) : IIntegratedExpression()
{
    this->structAnalyzer = structAnalyzer;
    this->substanceIndex = structAnalyzer->getSubstanceIndex(substanceName);
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::IntegratedConcentration::~IntegratedConcentration()
{
    //  Nothiong to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
bool BIO_TRD_NS::IntegratedConcentration::isDefined(int h, int v)
{
    std::vector<int> s = structAnalyzer->getSubstanceIndexesInArea(h, v);
    return std::find(s.begin(), s.end(), substanceIndex) != s.end();
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_TRD_NS::IntegratedConcentration::getValue(BIO_DM_NS::IConcentrations* c)
{
    return c->getConcentration(substanceIndex);
}


/* ************************************************************************** */
/* ************************************************************************** */
