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
