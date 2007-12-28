#ifndef BIO_DM_IGrid1D_HXX
#define BIO_DM_IGrid1D_HXX
#include "../../biosensor.hxx"
#include "IGrid.hxx"
#include "ICursor1D.hxx"
BIO_DM_NS_BEGIN


/**
 *  Composite data model.
 */
class IGrid1D : public IGrid
{
public:
    virtual int getPointCount() = 0;
    virtual double* getPointPositions() = 0;
    virtual ICursor1D* newGridCursor() = 0;
};


BIO_DM_NS_END
#endif
