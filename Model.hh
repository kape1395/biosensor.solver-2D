/* ************************************************************************** */
/** \namespace cfg
 *  Klases, kuriomis apibudinama, koki biojutikli reikia modeliuoti.
 */
/* ************************************************************************** */
/** \namespace dm
 *  Klases, apibreciancios duomenu strukturas.
 */
/* ************************************************************************** */
/** \namespace sa
 *  Sprendeju algoritmai.
 */
/* ************************************************************************** */

#ifndef Model_HH
#define Model_HH

class Dimension;

#include "dm/Abstract.hh"



/* ************************************************************************** */
/* ************************************************************************** */
/*
 *  Krypciu enumeracija.
 *
enum Direction
{
    HORIZONTAL = 0, H = 0, X = 0,
    VERTICAL   = 1, V = 1, Y = 1
};
 */



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Dimensijos atkarpa.
 */
class Dimension
{
public:
    /**
     *  Krypciu enumeracija.
     */
    enum Direction
    {
        HORIZONTAL = 0,
        VERTICAL   = 1
    };

protected:
    Direction type;       ///< 0 - horizontali, 1 - vertikali.
    double    length;      ///< Dimensijos atkarpos ilgis.
    double    start;      ///< Dimensijos pradzia.

public:
    Dimension(
        Direction   type,
        double      start,
        double      length
    );

    virtual ~Dimension();
    virtual int      getPointCount() = 0;
    virtual double   getLength() = 0;
    virtual double*  getPositions() = 0;
    virtual double*  getIntervals() = 0;

};



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Dimensijos atkarpa.
 */
class ConstantDimension : public Dimension
{
protected:
    int     steps;
    double  *positions;
    double  *intervals;

public:
    ConstantDimension(
        Direction   type,
        double      start,
        double      length,
        int         steps
    );

    virtual ~ConstantDimension();
    virtual int      getPointCount();
    virtual double   getLength();
    virtual double*  getPositions();
    virtual double*  getIntervals();

};



/* ************************************************************************** */
/* ************************************************************************** */
#endif
