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
#include "ConstantCondition.hxx"
#include <bio/Logging.hxx>
#include <bio/Exception.hxx>
#include <cmath>
#define LOGGER "libbiosensor-slv-fd::im2d::ConstantCondition: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::ConstantCondition::ConstantCondition(
    IAreaEdgeData* edge,
    double concentration,
    bool atStart
)
{
    LOG_DEBUG(LOGGER << "ConstantCondition()...");

    this->edge = edge;
    this->concentration = concentration;
    this->atStart = atStart;
    applyInitialValues();

    LOG_DEBUG(LOGGER << "ConstantCondition()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::ConstantCondition::~ConstantCondition()
{
    // Nothing to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::ConstantCondition::solveThroughForward()
{
    if (atStart) // P and Q are needed at start only
    {
        for (int i = 1; i < edge->getSize() - 1; i++)
        {
            edge->setP0(i, 0.0);
            edge->setQ0(i, concentration);
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::ConstantCondition::solveThroughBackward()
{
    if (atStart)
    {
        for (int i = 1; i < edge->getSize() - 1; i++)
        {
            edge->setC0(i, edge->getP0(i) * edge->getC1(i) + edge->getQ0(i));
        }
    }
    else
    {
        for (int i = 1; i < edge->getSize() - 1; i++)
        {
            edge->setC0(i, concentration);
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::ConstantCondition::solveAlongForward()
{
    //  Nothing to do here (for now the explicit aproach is used "along").
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::ConstantCondition::solveAlongBackward()
{
    //for (int i = 1; i < edge->getSize() - 1; i++)
    for (int i = 0; i < edge->getSize(); i++)
    {
        edge->setC0(i, concentration);
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::ConstantCondition::applyInitialValues()
{
    solveAlongBackward();
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_SLV_FD_IM2D_NS::ConstantCondition::getConcentration(int x)
{
    return edge->getC0(x);
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::ConstantCondition::setConcentration(int x, double c)
{
    if (std::abs(concentration - c) > ZERO_MAX)
    {
        LOG_ERROR(LOGGER
                  << "Trying to set invalid concentration for a bound with constant condition:"
                  << " new=" << c << " const=" << concentration
                 );
        throw BIO_NS::Exception("Trying to set concentration for a bound with constant condition...");
    }
    edge->setC0(x, concentration);
}


/* ************************************************************************** */
/* ************************************************************************** */
