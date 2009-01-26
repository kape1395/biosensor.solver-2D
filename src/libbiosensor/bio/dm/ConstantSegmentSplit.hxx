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
    ) : ISegmentSplit()
    {
        this->start = start;
        this->length = length;
        this->stepCount = stepCount;
    }

    /**
     *  Destructor.
     */
    virtual ~ConstantSegmentSplit()
    {
        //  Empty virtual destructor.
    }

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
        return (length / stepCount) * i;
    }

    virtual double getStepLength(int i)
    {
        return length / stepCount;
    }

};



BIO_DM_NS_END

#endif
