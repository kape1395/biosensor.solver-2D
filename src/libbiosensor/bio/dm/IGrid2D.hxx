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
