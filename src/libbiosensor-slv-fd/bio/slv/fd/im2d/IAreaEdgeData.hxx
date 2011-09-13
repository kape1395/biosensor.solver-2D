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
#ifndef BIO_SLV_FD_IM2D_IAreaEdgeData_HXX
#define BIO_SLV_FD_IM2D_IAreaEdgeData_HXX
#include "../../../../biosensor-slv-fd.hxx"

BIO_SLV_FD_IM2D_NS_BEGIN


class IAreaEdgeData
{
public:
    virtual ~IAreaEdgeData()
    {
        //  nothing.
    }

    virtual int getSize() = 0;
    virtual double getStepSize() = 0;

    /**
     *  returns thrue, pos(0) &lt; pos[1] (usually its on left and top edge).
     */
    virtual bool isForward() = 0;

    virtual void setP0(int index, double value) = 0;
    virtual double getP0(int index) = 0;
    virtual double getP1(int index) = 0;
    virtual void setQ0(int index, double value) = 0;
    virtual double getQ0(int index) = 0;
    virtual double getQ1(int index) = 0;
    virtual void setC0(int index, double value) = 0;
    virtual double getC0(int index) = 0;
    virtual double getC1(int index) = 0;

};

BIO_SLV_FD_IM2D_NS_END

#endif
