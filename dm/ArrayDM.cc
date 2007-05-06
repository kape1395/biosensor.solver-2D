#include "ArrayDM.hh"
namespace dm
{


Area* ArrayModelFactory::newArea(
    cfg::Area*      configuration,
    PointFactory*   pointFactory,
    Dimension*      dimX,
    Dimension*      dimY
)
{
    return new ArrayArea(configuration, pointFactory, dimX, dimY);
}


Bound* ArrayModelFactory::newBound(
    cfg::Bound*     configuration,
    PointFactory*   pointFactory,
    Dimension*      dim,
    Area*           prev,
    Area*           next
)
{
    return new ArrayBound(configuration, pointFactory, dim, prev, next);
}


Corner* ArrayModelFactory::newCorner(
    PointFactory*   pointFactory,
    Bound*          top,
    Bound*          right,
    Bound*          bottom,
    Bound*          left
)
{
    return new ArrayCorner(pointFactory, top, right, bottom, left);
}



/* ************************************************************************** */
/**
 *  Konstruktorius.
 */
ArrayArea::ArrayArea(
    cfg::Area*      configuration,
    PointFactory*   pointFactory,
    Dimension*      dimX,
    Dimension*      dimY
) : Area(configuration, dimX, dimY)
{
    sizeX = dimX->getPointCount();
    sizeY = dimY->getPointCount();
    posX = 0;
    posY = 0;

    data = new Point**[sizeX];
    for (int i = 0; i < sizeX; i++)
    {
        data[i] = new Point*[sizeY];
        for (int j = 0; j < sizeY; j++)
            data[i][j] = pointFactory->newPoint();
    }
}



/* ************************************************************************** */
/**
 *  Destruktorius.
 */
ArrayArea::~ArrayArea()
{
    for (int i = 0; i < sizeX; i++)
    {
        for (int j = 0; j < sizeY; j++)
            delete data[i][j];
        delete[] data[i];
    }
    delete[] data;
}




/* ************************************************************************** */
/**
 *  Kosntruktorius.
 */
ArrayBound::ArrayBound(
    cfg::Bound*     configuration,
    PointFactory*   pointFactory,
    Dimension*      dim,
    Area*           prev,
    Area*           next
) : Bound(configuration, dim, prev, next)
{
    size = dim->getPointCount();
    pos = 0;

    data = new Point*[size];
    for (int i = 0; i < size; i++)
        data[i] = pointFactory->newPoint();
}



/* ************************************************************************** */
/**
 *  Konstruktorius.
 */
ArrayBound::~ArrayBound()
{
    for (int i = 0; i < size; i++)
        delete data[i];
    delete[] data;
}



/* ************************************************************************** */
/**
 *  Konstruktorius.
 */
ArrayCorner::ArrayCorner(
    PointFactory*   pointFactory,
    Bound*          top,
    Bound*          right,
    Bound*          bottom,
    Bound*          left
) : Corner(top, right, bottom, left)
{
    data = pointFactory->newPoint();
}



/* ************************************************************************** */
/**
 *  Konstruktorius.
 */
ArrayCorner::~ArrayCorner()
{
    delete data;
}



/* ************************************************************************** */
/* ************************************************************************** */
}   // namespace dm
