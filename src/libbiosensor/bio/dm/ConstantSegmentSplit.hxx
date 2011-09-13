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
#ifndef BIO_DM_ConstantSegmentSplit_HXX
#define BIO_DM_ConstantSegmentSplit_HXX
#include "../../biosensor.hxx"
#include "ISegmentSplit.hxx"

BIO_DM_NS_BEGIN


/**
 *  SegmentSplit, that splits the segment into the equal parts
 *  (in constant steps).
 */
class ConstantSegmentSplit : public ISegmentSplit
{
private:
    double start;
    double length;
    int stepCount;

public:

    /**
     *  Constructor.
     *
     *  \param start        Start position of the segment.
     *  \param length       Lenght of the segment.
     *  \param stepCount    Number of steps (number of parts,
     *                      into which segment is divided).
     */
    ConstantSegmentSplit(
        double start,
        double length,
        int stepCount
    );

    /**
     *  Destructor.
     */
    virtual ~ConstantSegmentSplit();

    virtual int getPointCount();
    virtual double getStartPosition();
    virtual double getLength();
    virtual double getLocalPointPosition(int i);
    virtual double getStepSize(int i);

};



BIO_DM_NS_END

#endif
