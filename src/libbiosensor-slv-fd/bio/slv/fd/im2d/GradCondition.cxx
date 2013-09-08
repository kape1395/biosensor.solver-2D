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
#include "GradCondition.hxx"
#include <bio/Logging.hxx>
#define LOGGER "libbiosensor-slv-fd::im2d::GradCondition: "

/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::GradCondition::GradCondition(
    IAreaEdgeData* edge,
    bool atStart,
    double diffusion,
    IAreaEdgeFunction* function
)
{
    LOG_DEBUG(LOGGER << "GradCondition()");

    this->edge = edge;
    this->atStart = atStart;
    this->diffusion = diffusion;
    this->function = function;

    applyInitialValues();
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::GradCondition::~GradCondition()
{
    //  Nothing to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::GradCondition::solveThroughForward()
{
    if (atStart)    // P and Q are needed at start only
    {
        double coef = edge->getStepSize() / diffusion;  // = h/D
        for (int i = 1; i < edge->getSize() - 1; i++)
        {
            // b0 = -(D/h).
            // c0 = (D/h);
            // p0 = - (c0 / b0) = 1.
            // q0 = (f0 / b0) = -f0 / (D / h) = -f0 * h / D.
            edge->setP0(i, 1.0);
            edge->setQ0(i, -function->getValue(i) * coef);
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::GradCondition::solveThroughBackward()
{
    if (atStart)
    {
        for (int i = 1; i < edge->getSize() - 1; i++)
        {
            // y0 = p0 * y1 + q0.
            edge->setC0(i, edge->getP0(i) * edge->getC1(i) + edge->getQ0(i));
        }
    }
    else
    {
        double coef = - edge->getStepSize() / diffusion;
        for (int i = 1; i < edge->getSize() - 1; i++)
        {
            // a_N = -(D/h);    a' = 1;
            // b_N = (D/h)      b' = -1;
            // f_N = f;         f' = -f * h / D;
            // y_N = (f' - a' * q_{N-1}) / (a' * p_{N-1} + b')
            edge->setC0(i, (coef * function->getValue(i) - edge->getQ1(i)) / (edge->getP1(i) - 1));
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::GradCondition::solveAlongForward()
{
    //  Nothing to do here (for now the explicit aproach is used "along").
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::GradCondition::solveAlongBackward()
{
    // y_0 = y_1 - f * h/D;
    // y_N = y_{N-1} + f * h/D;
    double coef = (atStart ? -1.0 : 1.0) * edge->getStepSize() / diffusion;
    for (int i = 0; i < edge->getSize(); i++)
    {
        // y = y' + f * coef.
        edge->setC0(i, edge->getC1(i) + function->getValue(i) * coef);
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::GradCondition::applyInitialValues()
{
    solveAlongBackward();
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_SLV_FD_IM2D_NS::GradCondition::getConcentration(int x)
{
    return edge->getC0(x);
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::GradCondition::setConcentration(int x, double c)
{
    //  NOTE: Is it ok to set only one layer?
    //        Now I think it's ok.
    edge->setC0(x, c);
}


/* ************************************************************************** */
/* ************************************************************************** */
