#ifndef BIO_DM_Cursor2DWithoutBounds_HXX
#define BIO_DM_Cursor2DWithoutBounds_HXX
#include "../../biosensor.hxx"
#include "ICursor2D.hxx"
#include "IConcentrations.hxx"
BIO_DM_NS_BEGIN


/**
 *  This cursor uses another specified cursor but prevents the user to
 *  get values on the bound of the area.
 *
 *  Implementation is not a very efficient one.
 */
class Cursor2DWithoutBounds : public ICursor2D
{
private:
    ICursor2D& cursor;
    bool valid;

public:
    Cursor2DWithoutBounds(ICursor2D& baseCursor);
    virtual ~Cursor2DWithoutBounds();

    virtual void left();
    virtual void right();
    virtual void top();
    virtual void down();
    virtual void rowStart();
    virtual void rowEnd();
    virtual void colStart();
    virtual void colEnd();
    virtual bool isValid();
    virtual IConcentrations* getConcentrations();

protected:
    void updateValidity();

};



BIO_DM_NS_END

#endif
