#ifndef BIO_DM_ICursor2D_HXX
#define BIO_DM_ICursor2D_HXX
#include "../../biosensor.hxx"
#include "IConcentrations.hxx"
BIO_DM_NS_BEGIN


/**
 * 2D Cursor. It is used to navigate in 2D grid data model.
 */
class ICursor2D
{
public:
    virtual int left() = 0;
    virtual int right() = 0;
    virtual int top() = 0;
    virtual int down() = 0;
    virtual void rowStart() = 0;
    virtual void rowEnd() = 0;
    virtual void colStart() = 0;
    virtual void colEnd() = 0;
    virtual IConcentrations& operator *() = 0;
};



BIO_DM_NS_END

#endif
