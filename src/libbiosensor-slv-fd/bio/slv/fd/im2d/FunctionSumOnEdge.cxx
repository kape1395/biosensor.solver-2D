/*
 * Copyright 2013 Karolis Petrauskas
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
#include "FunctionSumOnEdge.hxx"
#include <bio/Logging.hxx>
#define LOGGER "libbiosensor-slv-fd::im2d::FunctionSumOnEdge: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::FunctionSumOnEdge::FunctionSumOnEdge(
    BIO_SLV_FD_IM2D_NS::IAreaEdgeFunction *func1,
    BIO_SLV_FD_IM2D_NS::IAreaEdgeFunction *func2,
    double coef
)
{
    this->func1 = func1;
    this->func2 = func2;
    this->coef = coef;

    LOG_DEBUG(LOGGER << "Created: coef=" << coef);
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::FunctionSumOnEdge::~FunctionSumOnEdge()
{
    //  Nothing to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
int BIO_SLV_FD_IM2D_NS::FunctionSumOnEdge::getSize()
{
    return func1->getSize();
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_SLV_FD_IM2D_NS::FunctionSumOnEdge::getValue(int index)
{
    return coef * (func1->getValue(index) + func2->getValue(index));
}


/* ************************************************************************** */
/* ************************************************************************** */
