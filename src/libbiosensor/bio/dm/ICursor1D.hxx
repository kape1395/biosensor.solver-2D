#ifndef BIO_DM_ICursor1D_HXX
#define BIO_DM_ICursor1D_HXX
#include "../../biosensor.hxx"
#include "IConcentrations.hxx"
BIO_DM_NS_BEGIN


/**
 * 1D Cursor. It is used to navigate in 1D grid data model.
 */
class ICursor1D
{
public:
    virtual int prev() = 0;
    virtual int next() = 0;
    virtual void start() = 0;
    virtual void end() = 0;
    virtual IConcentrations& operator *() = 0;
};



BIO_DM_NS_END

#endif
