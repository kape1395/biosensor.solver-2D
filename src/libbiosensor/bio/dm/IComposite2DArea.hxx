#ifndef BIO_DM_IComposite2DArea_HXX
#define BIO_DM_IComposite2DArea_HXX
#include "../../biosensor.hxx"
#include "IComposite2DBound.hxx"
#include "IGrid2D.hxx"
BIO_DM_NS_BEGIN


/**
 *  Composite data model.
 */
class IComposite2DArea : public IGrid2D
{
public:
    virtual IComposite2DBound* getTopBound() = 0;
    virtual IComposite2DBound* getRightBound() = 0;
    virtual IComposite2DBound* getBottomBound() = 0;
    virtual IComposite2DBound* getLeftBound() = 0;
};


BIO_DM_NS_END
#endif
