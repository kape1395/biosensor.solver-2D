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
#include "SubstanceConcOnEdge.hxx"
#include <bio/Logging.hxx>
#define LOGGER "libbiosensor-slv-fd::im2d::SubstanceConcOnEdge: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::SubstanceConcOnEdge::SubstanceConcOnEdge(
    BIO_SLV_FD_IM2D_NS::IAreaEdgeData* edgeData,
    double coef
)
{
    this->edgeData = edgeData;
    this->coef = coef;

    LOG_DEBUG(LOGGER << "Created: coef=" << coef << " isForward=" << edgeData->isForward());
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::SubstanceConcOnEdge::~SubstanceConcOnEdge()
{
    //  Nothing to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
int BIO_SLV_FD_IM2D_NS::SubstanceConcOnEdge::getSize()
{
    return edgeData->getSize();
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_SLV_FD_IM2D_NS::SubstanceConcOnEdge::getValue(int index)
{
    return coef * edgeData->getC0(index);
}


/* ************************************************************************** */
/* ************************************************************************** */
