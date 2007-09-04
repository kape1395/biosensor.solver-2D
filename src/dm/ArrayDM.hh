#ifndef DM_ArrayDM_HH
#define DM_ArrayDM_HH
#include "AbstractDM.hh"


namespace dm
{
class ArrayModelFactory;
class ArrayArea;
class ArrayBound;
class ArrayCorner;



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Kuria Array tipo duomenu strukturas.
 */
class ArrayModelFactory : public ModelFactory
{
public:
    ArrayModelFactory() : ModelFactory()
    {}
    virtual ~ArrayModelFactory()
    {}

    /// Kuria nauja sriti.
    virtual Area* newArea(
        cfg::Area*      configuration,
        PointFactory*   pointFactory,
        Dimension*      dimX,
        Dimension*      dimY
    );

    /// Kuria nauja krasta.
    virtual Bound* newBound(
        cfg::Bound*     configuration,
        PointFactory*   pointFactory,
        Dimension*      dim,
        Area*           prev,
        Area*           next
    );

    /// Kuria nauja kampa.
    virtual Corner* newCorner(
        PointFactory*   pointFactory,
        Bound*          top,
        Bound*          right,
        Bound*          bottom,
        Bound*          left
    );

};



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Srities vidus, realizuotas masyvais.
 */
class ArrayArea : public Area
{
    friend class ArrayBound;
    friend class ArrayCorner;
protected:
    Point ***data;  ///< Dvimatis masyvas.
    int sizeX;
    int sizeY;
    int posX;
    int posY;

public:

    /// Konstruktorius.
    ArrayArea(
        cfg::Area*      configuration,
        PointFactory*   pointFactory,
        Dimension*      dimX,
        Dimension*      dimY
    );

    /// Destruktorius.
    virtual ~ArrayArea();

    virtual int  moveTop()
    {
        return --posY;
    }
    virtual int  moveRight()
    {
        return sizeX - 1 - ++posX;
    }
    virtual int  moveBottom()
    {
        return sizeY - 1 - ++posY;
    }
    virtual int  moveLeft()
    {
        return --posX;
    }
    virtual void moveToColStart()
    {
        posY = 0;
    }
    virtual void moveToColEnd()
    {
        posY = sizeY - 1;
    }
    virtual void moveToRowStart()
    {
        posX = 0;
    }
    virtual void moveToRowEnd()
    {
        posX = sizeY - 1;
    }
    virtual Point* getTop()
    {
        return data[posX][posY - 1];
    }
    virtual Point* getRight()
    {
        return data[posX + 1][posY];
    }
    virtual Point* getBottom()
    {
        return data[posX][posY + 1];
    }
    virtual Point* getLeft()
    {
        return data[posX - 1][posY];
    }
    virtual Point* getCurrent()
    {
        return data[posX][posY];
    }

};



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Sriciu sandura, realizuota masyvais.
 */
class ArrayBound : public Bound
{
    friend class ArrayCorner;
protected:
    Point **data;   ///< Vienmatis masyvas.
    int size;
    int pos;

    Point** nextAreaPoint;  ///< Pointeriai i sekancia nuo krasto tasku eilute/stupeli
    Point** prevAreaPoint;  ///< Pointeriai i sekancia nuo krasto tasku eilute/stupeli

public:
    ArrayBound(
        cfg::Bound*     configuration,
        PointFactory*   pointFactory,
        Dimension*      dim,
        Area*           prev,
        Area*           next
    );

    virtual ~ArrayBound();

    virtual int  moveNext()
    {
        return size - 1 - ++pos;
    }
    virtual int  movePrev()
    {
        return --pos;
    }
    virtual void moveToStart()
    {
        pos = 0;
    }
    virtual void moveToEnd()
    {
        pos = size - 1;
    }
    virtual Point* getNext()
    {
        return data[pos + 1];
    }
    virtual Point* getPrev()
    {
        return data[pos - 1];
    }
    virtual Point* getCurrent()
    {
        return data[pos];
    }

    virtual Point* getNextAreaPoint()
    {
        return nextAreaPoint[pos];
    }

    virtual Point* getPrevAreaPoint()
    {
        return prevAreaPoint[pos];
    }

};



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Sriciu susidurimo kampas, realizuotas masyvais (nera cia masyvu :) ).
 */
class ArrayCorner : public Corner
{
protected:
    Point* data;

public:
    ArrayCorner(
        PointFactory*   pointFactory,
        Bound*          top,
        Bound*          right,
        Bound*          bottom,
        Bound*          left
    );
    virtual ~ArrayCorner();

    virtual Point * getCurrent()
    {
        return data;
    }
};



/* ************************************************************************** */
/* ************************************************************************** */
}   // namespace dm
#endif
