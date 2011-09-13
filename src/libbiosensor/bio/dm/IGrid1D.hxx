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
#ifndef BIO_DM_IGrid1D_HXX
#define BIO_DM_IGrid1D_HXX
#include "../../biosensor.hxx"
#include "IGrid.hxx"
#include "ICursor1D.hxx"
#include "ISegmentSplit.hxx"
BIO_DM_NS_BEGIN


/**
 *  Composite data model.
 */
class IGrid1D : public IGrid
{
public:
    virtual ~IGrid1D()
    {
        //  Empty virtual destructor.
    }

    virtual ISegmentSplit* getPointPositions() = 0;
    virtual ICursor1D* newGridCursor() = 0;
};


BIO_DM_NS_END
#endif
