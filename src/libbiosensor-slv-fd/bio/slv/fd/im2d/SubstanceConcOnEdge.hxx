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
#ifndef BIO_SLV_FD_IM2D_SubstanceConcOnEdge_HXX
#define BIO_SLV_FD_IM2D_SubstanceConcOnEdge_HXX
#include "../../../../biosensor-slv-fd.hxx"
#include "IAreaEdgeFunction.hxx"
#include "IAreaEdgeData.hxx"
BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *  This class calculates a concentration for the substance on the bound of the area.
 */
class SubstanceConcOnEdge : public BIO_SLV_FD_IM2D_NS::IAreaEdgeFunction
{
    BIO_SLV_FD_IM2D_NS::IAreaEdgeData* edgeData;
    double coef;    //!< Just a precalculated static coefficient.

public:

    /**
     *  Constructor.
     *  @param edgeData concentrations will be taken fron this source.
     *  @param diffusionCoef Dissusion coefficient of the substance.
     */
    SubstanceConcOnEdge(
        BIO_SLV_FD_IM2D_NS::IAreaEdgeData* edgeData,
        double coef
    );

    /**
     *  Destructor.
     */
    virtual ~SubstanceConcOnEdge();

    virtual int getSize();

    virtual double getValue(int index);

};

BIO_SLV_FD_IM2D_NS_END

#endif
