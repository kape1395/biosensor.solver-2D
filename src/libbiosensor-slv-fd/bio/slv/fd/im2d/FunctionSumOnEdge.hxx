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
#ifndef BIO_SLV_FD_IM2D_FunctionSumOnEdge_HXX
#define BIO_SLV_FD_IM2D_FunctionSumOnEdge_HXX
#include "../../../../biosensor-slv-fd.hxx"
#include "IAreaEdgeFunction.hxx"
#include "IAreaEdgeData.hxx"
BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *  This class calculates a sum of two functions?
 */
class FunctionSumOnEdge : public BIO_SLV_FD_IM2D_NS::IAreaEdgeFunction
{
    BIO_SLV_FD_IM2D_NS::IAreaEdgeFunction *func1;
    BIO_SLV_FD_IM2D_NS::IAreaEdgeFunction *func2;
    double coef;    //!< Just a precalculated static coefficient.

public:

    /**
     *  Constructor.
     */
    FunctionSumOnEdge(
        BIO_SLV_FD_IM2D_NS::IAreaEdgeFunction *func1,
        BIO_SLV_FD_IM2D_NS::IAreaEdgeFunction *func2,
        double coef
    );

    /**
     *  Destructor.
     */
    virtual ~FunctionSumOnEdge();

    virtual int getSize();

    virtual double getValue(int index);

};

BIO_SLV_FD_IM2D_NS_END

#endif
