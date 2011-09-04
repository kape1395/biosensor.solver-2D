#ifndef BIO_DM_Cursor2DOpenBounds_HXX
#define BIO_DM_Cursor2DOpenBounds_HXX
#include "../../biosensor.hxx"
#include "ICursor2D.hxx"
#include "IConcentrations.hxx"
BIO_DM_NS_BEGIN


/**
 *  TODO: Documentation.
 */
class Cursor2DOpenBounds : public ICursor2D
{
private:
    ICursor2D* cursor;
    bool autoDelete;
    int sizeH;
    int sizeV;
    int posH;
    int posV;

public:
    Cursor2DOpenBounds(ICursor2D* baseCursor, bool autoDelete);
    virtual ~Cursor2DOpenBounds();

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

};



BIO_DM_NS_END

#endif
