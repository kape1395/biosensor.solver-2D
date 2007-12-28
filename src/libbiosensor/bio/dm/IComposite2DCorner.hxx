#ifndef BIO_DM_IComposite2DCorner_HXX
#define BIO_DM_IComposite2DCorner_HXX
#include "../../biosensor.hxx"
#include "IComposite2DBound.hxx"
#include "IConcentrations.hxx"
BIO_DM_NS_BEGIN


/**
 *  Composite data model.
 */
class IComposite2DCorner
{
public:
    virtual IComposite2DBound* getTopBound() = 0;
    virtual IComposite2DBound* getRightBound() = 0;
    virtual IComposite2DBound* getBottomBound() = 0;
    virtual IComposite2DBound* getLeftBound() = 0;
    virtual IConcentrations* getConcentrations() = 0;
};


BIO_DM_NS_END
#endif
