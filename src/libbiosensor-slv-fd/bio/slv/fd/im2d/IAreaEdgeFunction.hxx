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
#ifndef BIO_SLV_FD_IM2D_IAreaEdgeFunction_HXX
#define BIO_SLV_FD_IM2D_IAreaEdgeFunction_HXX
#include "../../../../biosensor-slv-fd.hxx"

BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *  It's an interface used to provifr functions for the bound conditions.
 */
class IAreaEdgeFunction
{
public:
    virtual ~IAreaEdgeFunction()
    {
        //  nothing.
    }

    /**
     *  Get "length" of this function. I.e. #getValue will accept
     *  indexes from range [0..size).
     */
    virtual int getSize() = 0;

    /**
     *  Returns a function value at the particular position
     *  specified by the index.
     */
    virtual double getValue(int index) = 0;

};

BIO_SLV_FD_IM2D_NS_END

#endif
