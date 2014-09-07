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
#ifndef BIO_SLV_FD_IM2D_ReactionFoulingOnEdge_HXX
#define BIO_SLV_FD_IM2D_ReactionFoulingOnEdge_HXX
#include "../../../../biosensor-slv-fd.hxx"
#include "IAreaEdgeFunction.hxx"
#include "IAreaEdgeData.hxx"
BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *  This class applies integral function to the specified function.
 *
 *  f = alpha * (beta + gamma * integral(0, t, f'))
 */
class TimeIntegralOnEdge : public BIO_SLV_FD_IM2D_NS::IAreaEdgeFunction
{
    IAreaEdgeFunction *function;
    ISolverState      *solverState;
    double            alpha;
    double            beta;
    double            gamma;
    double*           integralValue;
    double*           integralPrev;
    double*           integralTime;

public:

    /**
     *  Constructor.
     */
    TimeIntegralOnEdge(
        IAreaEdgeFunction *function,
        ISolverState      *solverState,
        double            alpha,
        double            beta,
        double            gamma
    );

    /**
     *  Destructor.
     */
    virtual ~TimeIntegralOnEdge();

    virtual int getSize();

    virtual double getValue(int index);

};

BIO_SLV_FD_IM2D_NS_END

#endif
