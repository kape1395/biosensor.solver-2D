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
#ifndef BIO_SLV_FD_IM2D_MergeCondition_HXX
#define BIO_SLV_FD_IM2D_MergeCondition_HXX
#include "../../../../biosensor-slv-fd.hxx"
#include "IBoundCondition.hxx"
#include "IAreaEdgeFunction.hxx"
#include "AreaSubSolver.hxx"

BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *  Bound condition "Merge".
 *  NOTE: This condition works only for cartesiant coordinate system.
 *
 *  The step "through" is solves using implicit scheme and the step "along" is
 *  solved using explicit scheme. This is because the boundary condition is
 *  defined in one direction only. I thing this is subject to change.
 */
class MergeCondition : public IBoundCondition
{
private:

    IAreaEdgeData* edgePrev;
    IAreaEdgeData* edgeNext;
    double diffusionPrev;
    double diffusionNext;
    IAreaEdgeFunction* function;
    int size;

    double a;   //!< Equation coefficient "a" (for the step "Through" and "Along").
    double b;   //!< Equation coefficient "b" (for the step "Through" and "Along").
    double c;   //!< Equation coefficient "c" (for the step "Through" and "Along").
    //double f;   //!< Equation coefficient "f" (for the step "Through" only).

public:

    /**
     *  Constructor.
     *
     *  \param edgePrev         Reference to the data in the previous area.
     *  \param edgeNext         Reference to the data in the next area.
     *  \param diffusionPrev    Diffusion coefficient in the previous area.
     *  \param diffusionNext    Diffusion coefficient in the next area.
     *  \param function         function to be used in calculations.
     */
    MergeCondition(
        IAreaEdgeData* edgePrev,
        IAreaEdgeData* edgeNext,
        double diffusionPrev,
        double diffusionNext,
        IAreaEdgeFunction* function
    );

    /**
     *
     */
    virtual ~MergeCondition();

    /**
     *
     */
    virtual void solveThroughForward();

    /**
     *
     */
    virtual void solveThroughBackward();

    /**
     *
     */
    virtual void solveAlongForward();

    /**
     *
     */
    virtual void solveAlongBackward();

    /**
     *
     */
    virtual void applyInitialValues();

    /**
     *
     */
    virtual double getConcentration(int x);

    /**
     *  Set concentration for the substance at specified point.
     *
     *  \param x    Point index;
     *  \param c    New concentration.
     */
    virtual void setConcentration(int x, double c);

};



BIO_SLV_FD_IM2D_NS_END

#endif
