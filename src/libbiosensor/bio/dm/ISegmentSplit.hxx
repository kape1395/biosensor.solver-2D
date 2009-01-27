#ifndef BIO_DM_ISegmentSplit_HXX
#define BIO_DM_ISegmentSplit_HXX
#include "../../biosensor.hxx"

BIO_DM_NS_BEGIN


/**
 *  This is an interface for objects, that can split a segment in some way.
 *  Planned implementations are ConstantSegmentSplit and BilinearSegmentSplit.
 */
class ISegmentSplit
{
public:

    /**
     *  Destructor.
     */
    virtual ~ISegmentSplit()
    {
        //  Empty virtual destructor.
    }

    /**
     *  Returns number of points in the segment, including boundary points.
     */
    virtual int getPointCount() = 0;

    /**
     *  Returns number of intervals in the segment.
     *  This is equal to (#getPointCount() - 1).
     */
    virtual int getStepCount()
    {
        return getPointCount() - 1;
    }

    /**
     *  Returns start position, at which the segment starts.
     */
    virtual double getStartPosition() = 0;

    /**
     *  Returns a lenght of the segment.
     */
    virtual double getLength() = 0;

    /**
     *  Returns the global position of a i^th point in the segment.
     *  This is equal to (#getStartPosition() + #getLocalPointPosition(int)).
     */
    virtual double getPointPosition(int i)
    {
        return getStartPosition() + getLocalPointPosition(i);
    }

    /**
     *  Get position of a point, locally in the segment.
     */
    virtual double getLocalPointPosition(int i) = 0;

    /**
     *  Get lenght of an i^th step.
     */
    virtual double getStepSize(int i) = 0;

};



BIO_DM_NS_END

#endif
