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
#include "CompositeSegmentSplit.hxx"
#include "../Exception.hxx"
#include "../Logging.hxx"
#define LOGGER "libbiosensor::CompositeSegmentSplit: "

/* ************************************************************************** */
BIO_DM_NS::CompositeSegmentSplit::CompositeSegmentSplit(
    std::vector<ISegmentSplit*>& subSegments
)
{

    if (subSegments.size() == 0)
        throw Exception("I cannot create CompositeSegmentSplit out of 0 compartments.");

    this->start = subSegments[0]->getStartPosition();
    this->length = 0.0;
    this->stepCount = 0;

    for (std::vector<ISegmentSplit*>::iterator split = subSegments.begin(); split < subSegments.end(); split++)
    {
        LOG_DEBUG(LOGGER << "sequential segments test: start=" << start << " length=" << length << " startPos=" << (*split)->getStartPosition() << "==" << (start + length));
        //  No bot check for equality. Use relative errors instead
        if ((((start + length) - (*split)->getStartPosition()) / length) > ZERO_MAX)
            throw Exception("SubSogments in CompositeSegmentSplit must go sequentialy and not interruptably.");

        length += (*split)->getLength();
        stepCount += (*split)->getStepCount();
    }


    localPointPositions = new double[getPointCount()];
    int i = 0;
    for (std::vector<ISegmentSplit*>::iterator split = subSegments.begin(); split < subSegments.end(); split++)
    {
        for (int j = 0; j < (*split)->getPointCount(); j++, i++)
        {
            localPointPositions[i] = (*split)->getPointPosition(j) - start;
        }
        i--;
    }
}


/* ************************************************************************** */
BIO_DM_NS::CompositeSegmentSplit::~CompositeSegmentSplit()
{
    delete [] localPointPositions;
}


/* ************************************************************************** */
