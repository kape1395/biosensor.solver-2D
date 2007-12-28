#ifndef BIO_DM_IGrid2D_HXX
#define BIO_DM_IGrid2D_HXX
#include "../../biosensor.hxx"
#include "IGrid.hxx"
#include "ICursor2D.hxx"

BIO_DM_NS_BEGIN


/**
 *  Interface for data models based on grid, for 2D solvers.
 */
class IGrid2D : public IGrid
{
public:
    virtual int getPointCountH() = 0;
    virtual int getPointCountV() = 0;
    virtual double* getPointPositionsH() = 0;
    virtual double* getPointPositionsV() = 0;
    virtual ICursor2D* newGridCursor() = 0;
};



BIO_DM_NS_END

#endif
