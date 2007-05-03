#ifndef DM_Array_HH
#define DM_Array_HH
#include "Abstract.hh"


namespace dm
{


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Kuria Array tipo duomenu strukturas.
 */
template<class SA>
class ArrayFactory : public Factory<SA>
{
public:
    ArrayFactory();
    virtual ~ArrayFactory();

    virtual Area<SA>* newArea(
        Dimension *dimX,
        Dimension *dimY
    );

    virtual Bound<SA>* newBound(
        Dimension *dim,
        Area<SA> *prev,
        Area<SA> *next
    );

    virtual Corner<SA>* newCorner(
        Bound<SA> *top,
        Bound<SA> *right,
        Bound<SA> *bottom,
        Bound<SA> *left
    );


};



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Srities vidus, realizuotas masyvais.
 */
template<class SA>
class ArrayArea : public Area<SA> {
protected:
    SA **data;  ///< Dvimatis masyvas.
    int sizeX;
    int sizeY;
    int posX;
    int posY;

public:
    ArrayArea(
        Dimension *dimX,
        Dimension *dimY
    );
    virtual ~ArrayArea();

    virtual int  moveTop();
    virtual int  moveRight();
    virtual int  moveBottom();
    virtual int  moveLeft();
    virtual void moveToColStart();
    virtual void moveToColEnd();
    virtual void moveToRowStart();
    virtual void moveToRowEnd();
    virtual SA&  getTop();
    virtual SA&  getRight();
    virtual SA&  getBottom();
    virtual SA&  getLeft();
    virtual SA&  getCurrent();

};



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Sriciu sandura, realizuota masyvais.
 */
template<class SA>
class ArrayBound : public Bound<SA> {
protected:
    SA *data;   ///< Vienmatis masyvas.

public:
    ArrayBound(
        Dimension *dim,
        Area<SA> *prev,
        Area<SA> *next
    );
    virtual ~ArrayBound();

};



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Sriciu susidurimo kampas, realizuotas masyvais (nera cia masyvu :) ).
 */
template<class SA>
class ArrayCorner : public Corner<SA> {
protected:
    SA data;

public:
    ArrayCorner(
        Bound<SA> *top,
        Bound<SA> *right,
        Bound<SA> *bottom,
        Bound<SA> *left
    );
    virtual ~ArrayCorner();

};



/* ************************************************************************** */
/* ************************************************************************** */
}   // namespace dm



//
//  Include template definitions.
//
#ifndef DM_Array_CC
#define DM_Array_TT
#include "Array.cc"
#endif

#endif
