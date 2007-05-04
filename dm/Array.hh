#ifndef DM_Array_HH
#define DM_Array_HH
#include "Abstract.hh"


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
        PointFactory*   pointFactory,
        Dimension*      dimX,
        Dimension*      dimY
    );

    /// Kuria nauja krasta.
    virtual Bound* newBound(
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
protected:
    Point ***data;  ///< Dvimatis masyvas.
    int sizeX;
    int sizeY;
    int posX;
    int posY;

public:

    /// Konstruktorius.
    ArrayArea(
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
protected:
    Point **data;   ///< Vienmatis masyvas.
    int size;
    int pos;

public:
    ArrayBound(
        PointFactory*   pointFactory,
        Dimension*      dim,
        Area*           prev,
        Area*           next
    );

    virtual ~ArrayBound();

    virtual int  moveNext()
    {
        return --pos;
    }
    virtual int  movePrev()
    {
        return size - 1 - ++pos;
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
