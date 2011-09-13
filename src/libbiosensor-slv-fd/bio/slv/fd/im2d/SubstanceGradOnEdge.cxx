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
#include "SubstanceGradOnEdge.hxx"
#include <bio/Logging.hxx>
#define LOGGER "libbiosensor-slv-fd::im2d::SubstanceGradOnEdge: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::SubstanceGradOnEdge::SubstanceGradOnEdge(
    BIO_SLV_FD_IM2D_NS::IAreaEdgeData* edgeData,
    double diffusionCoef
)
{
    this->edgeData = edgeData;
    this->coef = diffusionCoef / edgeData->getStepSize();

    if (!edgeData->isForward())
    {
        coef = -coef;
    }

    LOG_DEBUG(LOGGER
              << "Created: diffusionCoef=" << diffusionCoef
              << " isForward=" << edgeData->isForward()
             );
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::SubstanceGradOnEdge::~SubstanceGradOnEdge()
{
    //  Nothing to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
int BIO_SLV_FD_IM2D_NS::SubstanceGradOnEdge::getSize()
{
    return edgeData->getSize();
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_SLV_FD_IM2D_NS::SubstanceGradOnEdge::getValue(int index)
{
    return coef * (edgeData->getC1(index) - edgeData->getC0(index));
}


/* ************************************************************************** */
/* ************************************************************************** */
