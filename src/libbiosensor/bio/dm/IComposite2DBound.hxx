#ifndef BIO_DM_IComposite2DBound_HXX
#define BIO_DM_IComposite2DBound_HXX
#include "../../biosensor.hxx"
#include "IGrid1D.hxx"

BIO_DM_NS_BEGIN
// Used classes (wf-decl instead of include)
class IComposite2DArea;


/**
 *  Composite data model.
 */
class IComposite2DBound : public IGrid1D
{
public:
    virtual ~IComposite2DBound()
    {
        //  Empty virtual destructor.
    }

    virtual IComposite2DArea* getNextArea() = 0;
    virtual IComposite2DArea* getPrevArea() = 0;
};


BIO_DM_NS_END
#endif
