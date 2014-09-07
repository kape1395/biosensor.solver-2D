/*
 * Copyright 2011-2014 Karolis Petrauskas
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
#include "TimeIntegralOnEdge.hxx"
#include <bio/Logging.hxx>
#define LOGGER "libbiosensor-slv-fd::im2d::TimeIntegralOnEdge: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::TimeIntegralOnEdge::TimeIntegralOnEdge(
    IAreaEdgeFunction *function,
    ISolverState      *solverState,
    double            alpha,
    double            beta,
    double            gamma
)
{
    this->function = function;
    this->solverState = solverState;
    this->alpha = alpha;
    this->beta = beta;
    this->gamma = gamma;

    int size = function->getSize();
    integralValue = new double[size];
    integralPrev  = new double[size];
    integralTime  = new double[size];
    for (int i = 0; i < size; i++) {
        integralValue[i] = 0.0;
        integralPrev[i]  = 0.0;
        integralTime[i]  = 0.0;
    }

    LOG_DEBUG(LOGGER << "Created");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::TimeIntegralOnEdge::~TimeIntegralOnEdge()
{
    delete [] integralValue;
    delete [] integralPrev;
    delete [] integralTime;
}


/* ************************************************************************** */
/* ************************************************************************** */
int BIO_SLV_FD_IM2D_NS::TimeIntegralOnEdge::getSize()
{
    return function->getSize();
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_SLV_FD_IM2D_NS::TimeIntegralOnEdge::getValue(int index)
{
    double funValue;
    double newValue;
    double thisTime;
    funValue = function->getValue(index);
    newValue = funValue * alpha * (beta + gamma * integralValue[index]);
    thisTime = solverState->getTime();
    if (thisTime > integralTime[index]) {
        integralValue[index] += (thisTime - integralTime[index]) * (integralPrev[index] + newValue) / 2.0;
        integralPrev[index] = newValue;
        integralTime[index] = thisTime;
    }
    return newValue;
}


/* ************************************************************************** */
/* ************************************************************************** */
