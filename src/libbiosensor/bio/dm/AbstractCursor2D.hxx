#ifndef BIO_DM_AbstractCursor2D_HXX
#define BIO_DM_AbstractCursor2D_HXX
#include "../../biosensor.hxx"
#include "../dm/ICursor2D.hxx"
BIO_DM_NS_BEGIN


/**
 *  Basic implementation for 2D cursor.
 *  Method getConcentrations() is left abstract.
 */
class AbstractCursor2D : public BIO_DM_NS::ICursor2D
{
protected:
    unsigned sizeH;         // Total size H
    unsigned sizeV;         // Total size V
    unsigned currentH;      // point index in H
    unsigned currentV;      // point index in V;

public:

    /**
     *  Constructor.
     */
    AbstractCursor2D(unsigned sizeH, unsigned sizeV)
    {
        this->sizeH = sizeH;
        this->sizeV = sizeV;
        this->currentH = 0;
        this->currentV = 0;
    }

    /**
     *  Destructor.
     */
    virtual ~AbstractCursor2D()
    {
        //  Nothing.
    }

    virtual void left()
    {
        --currentH;
    }
    virtual void right()
    {
        currentH++;
    }
    virtual void top()
    {
        currentV--;
    }
    virtual void down()
    {
        currentV++;
    }

    virtual void rowStart()
    {
        currentH = 0;
    }
    virtual void rowEnd()
    {
        currentH = sizeH - 1;
    }
    virtual void colStart()
    {
        currentV = 0;
    }
    virtual void colEnd()
    {
        currentV = sizeV - 1;
    }

    virtual bool isValid()
    {
        return currentH >= 0 && currentH < sizeH && currentV >= 0 && currentV < sizeV;
    }

};



BIO_DM_NS_END
#endif
