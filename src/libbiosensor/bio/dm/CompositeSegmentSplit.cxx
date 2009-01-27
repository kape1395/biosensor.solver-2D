#include "CompositeSegmentSplit.hxx"
#include "../Exception.hxx"

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
        if (start + length != (*split)->getStartPosition())
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
