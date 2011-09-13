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
#include "WallCondition.hxx"
#include <bio/Logging.hxx>
#define LOGGER "libbiosensor-slv-fd::im2d::WallCondition: "

/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::WallCondition::WallCondition(
    IAreaEdgeData* edge,
    bool atStart
)
{
    LOG_DEBUG(LOGGER << "WallCondition()");

    this->edge = edge;
    this->atStart = atStart;

    applyInitialValues();
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::WallCondition::~WallCondition()
{
    //  Nothing to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::WallCondition::solveThroughForward()
{
    if (atStart)    // P and Q are needed at start only
    {
        for (int i = 1; i < edge->getSize() - 1; i++)
        {
            edge->setP0(i, 1.0);
            edge->setQ0(i, 0.0);
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::WallCondition::solveThroughBackward()
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
            edge->setC0(i, -(edge->getQ1(i) / (edge->getP1(i) - 1)));
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::WallCondition::solveAlongForward()
{
    //  Nothing to do here (for now the explicit aproach is used "along").
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::WallCondition::solveAlongBackward()
{
    //for (int i = 1; i < edge->getSize() - 1; i++)
    for (int i = 0; i < edge->getSize(); i++)
    {
        edge->setC0(i, edge->getC1(i));
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::WallCondition::applyInitialValues()
{
    solveAlongBackward();
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_SLV_FD_IM2D_NS::WallCondition::getConcentration(int x)
{
    return edge->getC0(x);
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::WallCondition::setConcentration(int x, double c)
{
    //  NOTE: Is it ok to set only one layer?
    //        Now I think it's ok.
    edge->setC0(x, c);
}


/* ************************************************************************** */
/* ************************************************************************** */
