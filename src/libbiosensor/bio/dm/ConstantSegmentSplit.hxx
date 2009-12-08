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
