#include "Model.hh"


/* ************************************************************************** */
/* **********   Dimenion   ************************************************** */
/* ************************************************************************** */



/**
 *  Konstruktorius.
 */
Dimension::Dimension(
    Direction   type,
    double      start,
    double      length
)
{
    this->type   = type;
    this->start  = start;
    this->length = length;
}



/**
 *  Destruktorius.
 */
Dimension::~Dimension()
{
    // Nothing to do.
}



/* ************************************************************************** */
/* **********   ConstantDimenion   ****************************************** */
/* ************************************************************************** */



/**
 *  Konstruktorius.
 *  @param type     Tipas (H/V).
 *  @param start    Dimensijos pradzia.
 *  @param length   Dimensijos ilgis.
 *  @param steps    Kiek zinsniu srityje (tasku bus 1 daugiau).
 */
ConstantDimension::ConstantDimension(
    Direction   type,
    double      start,
    double      length,
    int         steps
) : Dimension(type, start, length)
{
    this->steps = steps;

    positions = new double[steps + 1];
    intervals = new double[steps];

    for (int i = 0; i < steps + 1; i++)
    {
        positions[i] = start + (length / steps * i);
    }

    for (int i = 0; i < steps; i++)
    {
        intervals[i] = positions[i + 1] - positions[i];
    }
}



/**
 *  Destruktorius.
 */
ConstantDimension::~ConstantDimension()
{
    delete[] positions;
    delete[] intervals;
}


/// @return Dimensijos tasku skaicius.
int ConstantDimension::getPointCount()
{
    return steps + 1;
}


/// @return Dimensijos ilgis.
double ConstantDimension::getLength()
{
    return length;
}


/// @return Dimensijos tasku pozicijos.
double* ConstantDimension::getPositions()
{
    return positions;
}


/// @return Dimensijos intervalu ilgiai.
double* ConstantDimension::getIntervals()
{
    return intervals;
}



/* ************************************************************************** */
/* ************************************************************************** */
/* ************************************************************************** */
