#ifndef BIO_DM_Cursor2DOpenBounds_HXX
#define BIO_DM_Cursor2DOpenBounds_HXX
#include "../../biosensor.hxx"
#include "ICursor2D.hxx"
#include "IConcentrations.hxx"
BIO_DM_NS_BEGIN


/**
 *  Decorator for a ICursor2D that makes boundaries of the corresponding
 *  area to be open. By term "open" this implementation means, that
 *  values on the boundaries are the same as one step from it.
 *  I.e. non-leakage conditions are applied.
 *
 *  This decorator was designed to be used when integrating reactions
 *  over areas, because integrating them on the boundaries with non-continuous
 *  concentrations is not valid (for example to take average of concentrations
 *  in the adjacent areas).
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
    /**
     * Constructor.
     * \param baseCursor Cursor to be decorated.
     * \param autoDelete True, if baseCursor should be deleted on destruction of this decorator.
     */
    Cursor2DOpenBounds(ICursor2D* baseCursor, bool autoDelete);

    /**
     * Destructor. Deletes baseCursor, if auto delete is requested.
     */
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
