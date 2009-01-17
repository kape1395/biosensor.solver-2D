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
    virtual void left() = 0;
    virtual void right() = 0;
    virtual void top() = 0;
    virtual void down() = 0;
    virtual void rowStart() = 0;
    virtual void rowEnd() = 0;
    virtual void colStart() = 0;
    virtual void colEnd() = 0;
    virtual bool isValid() = 0;
    virtual IConcentrations* getConcentrations() = 0;
};



BIO_DM_NS_END

#endif
