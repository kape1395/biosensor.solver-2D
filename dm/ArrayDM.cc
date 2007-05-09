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
    for (int i = 1; i < sizeX - 1; i++)
    {
        data[i] = new Point*[sizeY];
        for (int j = 1; j < sizeY - 1; j++)
            data[i][j] = pointFactory->newPoint();
    }
    data[0        ] = new Point*[sizeY];
    data[sizeX - 1] = new Point*[sizeY];
}



/* ************************************************************************** */
/**
 *  Destruktorius.
 */
ArrayArea::~ArrayArea()
{
    for (int i = 1; i < sizeX - 1; i++)
    {
        for (int j = 1; j < sizeY - 1; j++)
            delete data[i][j];
        delete[] data[i];
    }
    delete[] data[0        ];
    delete[] data[sizeX - 1];
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

    data          = new Point*[size];
    nextAreaPoint = new Point*[size];
    prevAreaPoint = new Point*[size];
    ArrayArea* nArea = dynamic_cast<ArrayArea*>(next);
    ArrayArea* pArea = dynamic_cast<ArrayArea*>(prev);

    for (int i = 1; i < size - 1; i++)  // krastu nekuriam, nes jie bus kampuose
    {
        data[i] = pointFactory->newPoint();
        switch (dim->getDirection())
        {
        case (dm::HORIZONTAL):
                        if (nArea != 0)
                {
                    nArea->data[i][0] = data[i];
                    nextAreaPoint[i] = nArea->data[i][1];
                }
            if (pArea != 0)
            {
                pArea->data[i][pArea->sizeY - 1] = data[i];
                prevAreaPoint[i] = pArea->data[i][pArea->sizeY - 2];
            }
            break;
        case (dm::VERTICAL):
                        if (nArea != 0)
                {
                    nArea->data[0][i] = data[i];
                    nextAreaPoint[i] = nArea->data[1][i];
                }
            if (pArea != 0)
            {
                pArea->data[pArea->sizeX - 1][i] = data[i];
                prevAreaPoint[i] = pArea->data[pArea->sizeX - 2][i];
            }
            break;
        default:
            std::cerr << "ERROR: Invalid dimension direction=" << dim->getDirection();
        }
    }
}



/* ************************************************************************** */
/**
 *  Konstruktorius.
 */
ArrayBound::~ArrayBound()
{
    for (int i = 1; i < size - 1; i++)
        delete data[i];
    delete[] data;
    delete[] nextAreaPoint;
    delete[] prevAreaPoint;
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

    if (top != 0)
    {
        ArrayBound* b = dynamic_cast<ArrayBound*>(top);
        b->data[b->size - 1] = data;
    }
    if (right != 0)
    {
        ArrayBound* b = dynamic_cast<ArrayBound*>(right);
        b->data[0] = data;
    }
    if (bottom != 0)
    {
        ArrayBound* b = dynamic_cast<ArrayBound*>(bottom);
        b->data[0] = data;
    }
    if (left != 0)
    {
        ArrayBound* b = dynamic_cast<ArrayBound*>(left);
        b->data[b->size - 1] = data;
    }


    if (top != 0 && top->getPrevArea() != 0)
    {
        ArrayArea* a = dynamic_cast<ArrayArea*>(top->getPrevArea());
        a->data[a->sizeX - 1][a->sizeY - 1] = data;
    }
    if (top != 0 && top->getNextArea() != 0)
    {
        ArrayArea* a = dynamic_cast<ArrayArea*>(top->getNextArea());
        a->data[0][a->sizeY - 1] = data;
    }
    if (bottom != 0 && bottom->getPrevArea() != 0)
    {
        ArrayArea* a = dynamic_cast<ArrayArea*>(bottom->getPrevArea());
        a->data[a->sizeX - 1][0] = data;
    }
    if (bottom != 0 && bottom->getNextArea() != 0)
    {
        ArrayArea* a = dynamic_cast<ArrayArea*>(bottom->getNextArea());
        a->data[0][0] = data;
    }
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
