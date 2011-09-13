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
#ifndef BIO_SLV_FD_IM2D_IBoundCondition_HXX
#define BIO_SLV_FD_IM2D_IBoundCondition_HXX
#include "../../../../biosensor-slv-fd.hxx"

BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *  Interface for boundary conditions.
 */
class IBoundCondition
{
public:

    /**
     *  Empty destructor.
     */
    virtual ~IBoundCondition()
    {
        // Nothing.
    };

    /**
     *  Solve equations forward (find P and Q) when equation direction
     *  is perpendicular to the bound direction.
     */
    virtual void solveThroughForward() = 0;

    /**
     *  Solve equations backward (find concentration C) when equation direction
     *  is perpendicular to the bound direction.
     */
    virtual void solveThroughBackward() = 0;

    /**
     *  Solve equations forward (find P and Q) when equation direction
     *  is parallel to the bound direction.
     */
    virtual void solveAlongForward() = 0;

    /**
     *  Solve equations backward (find concentration C) when equation direction
     *  is parallel to the bound direction.
     */
    virtual void solveAlongBackward() = 0;

    /**
     *  Apply initial values.
     */
    virtual void applyInitialValues() = 0;

    /**
     *  Returns concentration at the specified point in the bound for
     *  corresponding substance..
     *
     *  \param x Point index.
     */
    virtual double getConcentration(int x) = 0;

    /**
     *  Set concentration for the substance at specified point.
     *
     *  \param x    Point index;
     *  \param c    New concentration.
     */
    virtual void setConcentration(int x, double c) = 0;

};



BIO_SLV_FD_IM2D_NS_END

#endif
