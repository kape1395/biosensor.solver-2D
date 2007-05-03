// Template definitions must be in header, when using GCC
// >> warning: keyword ‘export’ not implemented, and will be ignored
#define DM_Array_CC
#include "Array.hh"
namespace dm
{


/* ************************************************************************** */
/* **********   ArrayFactory   ********************************************** */
/* ************************************************************************** */
#ifdef DM_Array_TT


/* ************************************************************************** */
/**
 *  Konstruktorius.
 */
template<class SA>
ArrayFactory<SA>::ArrayFactory() : Factory<SA>()
{
    // Nothing to do.
}



/* ************************************************************************** */
/**
 *  Konstruktorius.
 */
template<class SA>
ArrayFactory<SA>::~ArrayFactory()
{
    // Nothing to do.
}



/* ************************************************************************** */
/**
 *  Sukuria nauja Area objekta.
 */
template<class SA>
Area<SA>* ArrayFactory<SA>::newArea(
    Dimension *dimX,
    Dimension *dimY
)
{
    return new ArrayArea<SA>(dimX, dimY);
}



/* ************************************************************************** */
/**
 *  Sukuria nauja Bound objekta.
 */
template<class SA>
Bound<SA>* ArrayFactory<SA>::newBound(
    Dimension *dim,
    Area<SA> *prev,
    Area<SA> *next
)
{
    return new ArrayBound<SA>(dim, prev, next);
}



/* ************************************************************************** */
/**
 *  Sukuria nauja Corner objekta.
 */
template<class SA>
Corner<SA>* ArrayFactory<SA>::newCorner(
    Bound<SA> *top,
    Bound<SA> *right,
    Bound<SA> *bottom,
    Bound<SA> *left
)
{
    return new ArrayCorner<SA>(top, right, bottom, left);
}


#endif
/* ************************************************************************** */
/* **********   ArrayArea   ************************************************* */
/* ************************************************************************** */
#ifdef DM_Array_TT



/* ************************************************************************** */
/**
 *  Konstruktorius.
 */
template<class SA>
ArrayArea<SA>::ArrayArea(
    Dimension *dimX,
    Dimension *dimY
) : Area<SA>(dimX, dimY)
{
    sizeX = dimX->getPointCount();
    sizeY = dimY->getPointCount();
    posX = 0;
    posY = 0;

    data = new SA*[sizeX];
    for (int i = 0; i < sizeX; i++)
    {
        data[i] = new SA[sizeY];
    }
}



/* ************************************************************************** */
/**
 *  Destruktorius.
 */
template<class SA>
ArrayArea<SA>::~ArrayArea()
{
    for (int i = 0; i < sizeX; i++)
    {
        delete[] data[i];
    }
    delete[] data;
}


template<class SA>
int ArrayArea<SA>::moveTop()
{
    return --posY;
}


template<class SA>
int ArrayArea<SA>::moveRight()
{
    return sizeX - 1 - ++posX;
}


template<class SA>
int ArrayArea<SA>::moveBottom()
{
    return sizeY - 1 - ++posY;
}


template<class SA>
int ArrayArea<SA>::moveLeft()
{
    return --posX;
}


template<class SA>
void ArrayArea<SA>::moveToColStart()
{
    posY = 0;
}


template<class SA>
void ArrayArea<SA>::moveToColEnd()
{
    posY = sizeY - 1;
}


template<class SA>
void ArrayArea<SA>::moveToRowStart()
{
    posX = 0;
}


template<class SA>
void ArrayArea<SA>::moveToRowEnd()
{
    posX = sizeY - 1;
}


template<class SA>
SA& ArrayArea<SA>::getTop()
{
    return data[posX][posY - 1];
}


template<class SA>
SA& ArrayArea<SA>::getRight()
{
    return data[posX + 1][posY];
}


template<class SA>
SA& ArrayArea<SA>::getBottom()
{
    return data[posX][posY + 1];
}


template<class SA>
SA& ArrayArea<SA>::getLeft()
{
    return data[posX - 1][posY];
}


template<class SA>
SA& ArrayArea<SA>::getCurrent()
{
    return data[posX][posY];
}



#endif
/* ************************************************************************** */
/* **********   ArrayBound   ************************************************ */
/* ************************************************************************** */
#ifdef DM_Array_TT



/* ************************************************************************** */
/**
 *  Kosntruktorius.
 */
template <class SA>
ArrayBound<SA>::ArrayBound(
    Dimension *dim,
    Area<SA> *prev,
    Area<SA> *next
) : Bound<SA>(dim, prev, next)
{
    data = new SA[dim->getPointCount()];
}



/* ************************************************************************** */
/**
 *  Konstruktorius.
 */
template <class SA>
ArrayBound<SA>::~ArrayBound()
{
    delete[] data;
}


#endif
/* ************************************************************************** */
/* **********   ArrayCorner   *********************************************** */
/* ************************************************************************** */
#ifdef DM_Array_TT



/* ************************************************************************** */
/**
 *  Konstruktorius.
 */
template <class SA>
ArrayCorner<SA>::ArrayCorner(
    Bound<SA> *top,
    Bound<SA> *right,
    Bound<SA> *bottom,
    Bound<SA> *left
) : Corner<SA>(top, right, bottom, left)
{
    // Nothing to do.
}



/* ************************************************************************** */
/**
 *  Konstruktorius.
 */
template <class SA>
ArrayCorner<SA>::~ArrayCorner()
{
    // Nothing to do.
}



#endif
/* ************************************************************************** */
/* ************************************************************************** */
/* ************************************************************************** */
}   // namespace dm
