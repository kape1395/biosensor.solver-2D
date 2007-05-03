#ifndef DM_Abstract_HH
#define DM_Abstract_HH
#include <list>



namespace dm
{

template<class D>
class Factory;

template<class SA, int x, int y>
class Model;

template<class D>
class Area;

template<class D>
class Bound;

template<class D>
class Corner;

}



/* ************************************************************************** */
#include "../Model.hh"
#include "../sa/Abstract.hh"
/* ************************************************************************** */
namespace dm
{



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Gamykla, kurianti objektus, sudaroancius mdoelio duomenu struktura.
 */
template<class SA>
class Factory
{
public:
    Factory();
    virtual ~Factory();

    virtual Area<SA>* newArea(
        Dimension *dimX,
        Dimension *dimY
    ) = 0;

    virtual Bound<SA>* newBound(
        Dimension *dim,
        Area<SA> *prev,
        Area<SA> *next
    ) = 0;

    virtual Corner<SA>* newCorner(
        Bound<SA> *top,
        Bound<SA> *right,
        Bound<SA> *bottom,
        Bound<SA> *left
    ) = 0;

};


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Klase atitinkanti biojutiklio duomenu modeli.
 */
template<class SA, int x, int y>
class Model
{
    Factory<SA>*    factory;
    Dimension*      dimH[x];
    Dimension*      dimV[y];
    Area<SA>*       area[x][y];
    Bound<SA>*      boundH[x][y + 1];
    Bound<SA>*      boundV[x + 1][y];
    Corner<SA>*     corner[x + 1][y + 1];

public:
    Model(
        Factory<SA>* factory,
        Dimension*   (&dimH)[x],
        Dimension*   (&dimV)[y]
    );
    virtual ~Model();

};


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Srities vidus, abstrakti realizacija.
 *
 *  Dimensijas sis objektas suziko sukurimo metu, taciau krastai
 *  nusirodo tada, kai jie kuriami (krastai). Jie patys nurodo sriciai,
 *  kad jie bus jos krastai.
 */
template<class SA>
class Area
{
    friend class Bound<SA>;
protected:
    Dimension *dimX;        ///< Dimensija pagal erdves X koordinate
    Dimension *dimY;        ///< Dimensija pagal erdves Y koordinate
    Bound<SA> *boundTop;    ///< Srities krastas is virsaus.
    Bound<SA> *boundRight;  ///< Srities krastas is desines.
    Bound<SA> *boundBottom; ///< Srities krastas is apacios.
    Bound<SA> *boundleft;   ///< Srities krastas is kaires.

public:
    Area(
        Dimension *dimX,
        Dimension *dimY
    );
    virtual ~Area();

    virtual int  moveTop() = 0;
    virtual int  moveRight() = 0;
    virtual int  moveBottom() = 0;
    virtual int  moveLeft() = 0;
    virtual void moveToColStart() = 0;
    virtual void moveToColEnd() = 0;
    virtual void moveToRowStart() = 0;
    virtual void moveToRowEnd() = 0;
    virtual SA&  getTop() = 0;
    virtual SA&  getRight() = 0;
    virtual SA&  getBottom() = 0;
    virtual SA&  getLeft() = 0;
    virtual SA&  getCurrent() = 0;

};



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Sriciu krastai / sanduros, abstrakti realizacija.
 */
template<class SA>
class Bound
{
    friend class Corner<SA>;
protected:
    Dimension *dim;
    Area<SA> *prev;
    Area<SA> *next;

public:
    Bound(
        Dimension *dim,
        Area<SA> *prev,
        Area<SA> *next
    );
    virtual ~Bound();

};



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Kampo (kuriame susiduria keturios sritys) abstrakti realizacija.
 */
template<class SA>
class Corner
{
protected:
    Bound<SA> *top;
    Bound<SA> *right;
    Bound<SA> *bottom;
    Bound<SA> *left;

public:
    Corner(
        Bound<SA> *top,
        Bound<SA> *right,
        Bound<SA> *bottom,
        Bound<SA> *left
    );
    virtual ~Corner();

};



/* ************************************************************************** */
/* ************************************************************************** */
}  // namespace dm



//
//  Include template definitions.
//
#ifndef DM_Abstract_CC
#define DM_Abstract_TT
#include "Abstract.cc"
#endif

#endif
