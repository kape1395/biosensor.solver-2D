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
#ifndef BIO_DM_IGrid2D_HXX
#define BIO_DM_IGrid2D_HXX
#include "../../biosensor.hxx"
#include "IGrid.hxx"
#include "ICursor2D.hxx"
#include "ISegmentSplit.hxx"

BIO_DM_NS_BEGIN


/**
 *  Interface for data models based on grid, for 2D solvers.
 */
class IGrid2D : public IGrid
{
public:
    virtual ~IGrid2D()
    {
        //  Empty virtual destructor.
    }

    virtual ISegmentSplit* getPointPositionsH() = 0;
    virtual ISegmentSplit* getPointPositionsV() = 0;
    virtual ICursor2D* newGridCursor() = 0;
};



BIO_DM_NS_END

#endif
