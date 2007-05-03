#define DM_Abstract_CC
#include "Abstract.hh"


namespace dm
{



/* ************************************************************************** */
/* **********   Factory   *************************************************** */
/* ************************************************************************** */
#ifdef DM_Abstract_TT



/* ************************************************************************** */
/**
 *  Konstruktorius.
 */
template<class SA>
Factory<SA>::Factory()
{
    // Nothing to do.
}



/* ************************************************************************** */
/**
 *  Konstruktorius.
 */
template<class SA>
Factory<SA>::~Factory()
{
    // Nothing to do.
}



#endif
/* ************************************************************************** */
/* **********   Model   ***************************************************** */
/* ************************************************************************** */
#ifdef DM_Abstract_TT



/* ************************************************************************** */
/**
 *  Konstruktorius.
 */
template<class SA, int x, int y>
Model<SA, x, y>::Model(
    Factory<SA> *factory,
    Dimension*  (&dimH)[x],
    Dimension*  (&dimV)[y]
)
{
    this->factory = factory;

    for (int i = 0; i < x; i++)
        this->dimH[i] = dimH[i];

    for (int j = 0; j < y; j++)
        this->dimV[j] = dimV[j];

    for (int i = 0; i < x; i++)
        for (int j = 0; j < y; j++)
            area[i][j] = factory->newArea(dimH[i], dimV[j]);

    for (int i = 0; i < x; i++)
        for (int j = 0; j <= y; j++)
            boundH[i][j] = factory->newBound(
                               dimH[i],
                               j == 0 ? 0 : area[i][j - 1],
                               j == y ? 0 : area[i][j]
                           );

    for (int i = 0; i <= x; i++)
        for (int j = 0; j < y; j++)
            boundV[i][j] = factory->newBound(
                               dimV[i],
                               i == 0 ? 0 : area[i - 1][j],
                               i == x ? 0 : area[i][j]
                           );

    for (int i = 0; i <= x; i++)
        for (int j = 0; j <= y; j++)
            corner[i][j] = factory->newCorner(
                                j == 0 ? 0 : boundV[i][j - 1],
                                i == x ? 0 : boundH[i][j],
                                j == y ? 0 : boundV[i][j],
                                i == 0 ? 0 : boundH[i - 1][j]
                           );
}



/* ************************************************************************** */
/**
 *  Konstruktorius.
 */
template<class SA, int x, int y>
Model<SA, x, y>::~Model()
{
    for (int i = 0; i <= x; i++)
        for (int j = 0; j <= y; j++)
            delete corner[i][j];

    for (int i = 0; i < x; i++)
        for (int j = 0; j <= y; j++)
            delete boundH[i][j];

    for (int i = 0; i <= x; i++)
        for (int j = 0; j < y; j++)
            delete boundV[i][j];

    for (int i = 0; i < x; i++)
        for (int j = 0; j < y; j++)
            delete area[i][j];
}



#endif
/* ************************************************************************** */
/* **********   Area   ****************************************************** */
/* ************************************************************************** */
#ifdef DM_Abstract_TT



/**
 *  TODO: Document.
 */
template<class SA>
Area<SA>::Area(
    Dimension *dimX,
    Dimension *dimY
)
{
    this->dimX = dimX;
    this->dimY = dimY;
}



/**
 *  TODO: Document.
 */
template<class SA>
Area<SA>::~Area()
{
    // Nothing to do.
}



#endif
/* ************************************************************************** */
/* **********   Bound   ***************************************************** */
/* ************************************************************************** */
#ifdef DM_Abstract_TT



/**
 *  TODO: Document.
 */
template<class SA>
Bound<SA>::Bound(
    Dimension *dim,
    Area<SA> *prev,
    Area<SA> *next
)
{
    this->dim = dim;
    this->prev = prev;
    this->next = next;
}



/**
 *  TODO: Document.
 */
template<class SA>
Bound<SA>::~Bound()
{
    // Nothing to do.
}



#endif
/* ************************************************************************** */
/* **********   Corner   **************************************************** */
/* ************************************************************************** */
#ifdef DM_Abstract_TT



/**
 *  TODO: Document.
 */
template<class SA>
Corner<SA>::Corner(
    Bound<SA> *top,
    Bound<SA> *right,
    Bound<SA> *bottom,
    Bound<SA> *left
)
{
    this->top    = top;
    this->right  = right;
    this->bottom = bottom;
    this->left   = left;
}



/**
 *  TODO: Document.
 */
template<class SA>
Corner<SA>::~Corner()
{
    // Nothing to do.
}



#endif
/* ************************************************************************** */
/* ************************************************************************** */
/* ************************************************************************** */
}   // namespace dm
