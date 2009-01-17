#ifndef BIO_DM_IComposite2D_HXX
#define BIO_DM_IComposite2D_HXX
#include "../../biosensor.hxx"
#include "IDataModel.hxx"
#include "IComposite2DArea.hxx"
#include "IComposite2DBound.hxx"
#include "IComposite2DCorner.hxx"
BIO_DM_NS_BEGIN


/**
 *  Composite data model.
 */
class IComposite2D : public IDataModel
{
public:
    virtual ~IComposite2D()
    {
        //  Empty virtual destructor.
    }

    virtual int getPartCountH() = 0;
    virtual int getPartCountV() = 0;
    virtual IComposite2DArea* getArea(int x, int y) = 0;
    virtual IComposite2DBound* getBoundH(int x, int y) = 0;
    virtual IComposite2DBound* getBoundV(int x, int y) = 0;
    virtual IComposite2DCorner* getCorner(int x, int y) = 0;
};


BIO_DM_NS_END
#endif
