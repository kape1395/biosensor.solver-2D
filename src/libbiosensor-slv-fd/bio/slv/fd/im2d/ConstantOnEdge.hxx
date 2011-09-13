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
#ifndef BIO_SLV_FD_IM2D_ConstantOnEdge_HXX
#define BIO_SLV_FD_IM2D_ConstantOnEdge_HXX
#include "../../../../biosensor-slv-fd.hxx"
#include "IAreaEdgeFunction.hxx"
BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *  This class is a constant fonction on the bound of the area.
 */
class ConstantOnEdge : public BIO_SLV_FD_IM2D_NS::IAreaEdgeFunction
{
    int size;
    double value;

public:

    /**
     *  Constructor.
     *  @param size     Lenght of the edge.
     *  @param value    Constant value.
     */
    ConstantOnEdge(int size, double value);

    /**
     *  Destructor.
     */
    virtual ~ConstantOnEdge();

    virtual int getSize();

    virtual double getValue(int index);

};

BIO_SLV_FD_IM2D_NS_END

#endif
