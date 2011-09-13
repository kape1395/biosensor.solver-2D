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
#ifndef BIO_DM_CompositeSegmentSplit_HXX
#define BIO_DM_CompositeSegmentSplit_HXX
#include "../../biosensor.hxx"
#include "ISegmentSplit.hxx"
#include <vector>

BIO_DM_NS_BEGIN


/**
 *  SegmentSplit, that consists out of other segment splits.
 */
class CompositeSegmentSplit : public ISegmentSplit
{
private:
    double start;
    double length;
    int stepCount;
    double* localPointPositions;

public:

    /**
     *  Constructor.
     *
     *  \param subSegments Sub-segments, out of which the composite segment
     *                      consists. They must be ordered by positions.
     */
    CompositeSegmentSplit(
        std::vector<ISegmentSplit*>& subSegments
    );

    /**
     *  Destructor.
     */
    virtual ~CompositeSegmentSplit();

    virtual int getPointCount()
    {
        return stepCount + 1;
    }

    virtual double getStartPosition()
    {
        return start;
    }

    virtual double getLength()
    {
        return length;
    }

    virtual double getLocalPointPosition(int i)
    {
        return localPointPositions[i];
    }

    virtual double getStepSize(int i)
    {
        return localPointPositions[i + 1] - localPointPositions[i];
    }

};



BIO_DM_NS_END

#endif
