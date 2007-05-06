#ifndef DM_Abstract_HH
#define DM_Abstract_HH
#include <list>
#include "../Model.hh"
#include "../Config.hh"
namespace dm
{
/* ************************************************************************** */
/* ************************************************************************** */
class Point;
class PointFactory;
class Model;
class ModelFactory;
class Area;
class Bound;
class Corner;



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Vieno tasko duomenys modelyje.
 */
class Point
{
public:
    virtual ~Point()
    {}
    virtual double getSubstance(int substanceNr) = 0;
};



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Tasku gamintoju interfeisas.
 */
class PointFactory
{
public:
    virtual ~PointFactory()
    {}
    virtual Point* newPoint() = 0;
};



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Klase atitinkanti biojutiklio duomenu modeli.
 */
class Model
{
protected:
    typedef std::list<Dimension*> DimensionList;
    cfg::Config*  configuration;
    ModelFactory* modelFactory;
    int         partsH;     ///< X size
    int         partsV;     ///< Y size
    Dimension** dimH;       ///< Dimensions (by X)
    Dimension** dimV;       ///< Dimensions (by Y)
    Area***     area;       ///< 2D Array   [x][y]
    Bound***    boundH;     ///< 2D Array   [x][y+1]
    Bound***    boundV;     ///< 2D Array   [x+1][y]
    Corner***   corner;     ///< 2D Array   [x+1][y+1]

public:
    /// Kosntruktorius.
    Model(
        cfg::Config*    configuration,
        PointFactory*   pointFactory,
        ModelFactory*   modelFactory,
        int             partsH,
        int             partsV,
        DimensionList&  dimH,
        DimensionList&  dimV
    );

    /// Destruktorius.
    virtual ~Model();


    virtual int getPartsH()
    {
        return partsH;
    }

    virtual int getPartsV()
    {
        return partsV;
    }

    virtual Area*** getArea()
    {
        return area;
    }

    virtual Bound *** getBoundH()
    {
        return boundH;
    }

    virtual Bound *** getBoundV()
    {
        return boundV;
    }

    virtual Corner *** getCorner()
    {
        return corner;
    }

    virtual cfg::Config* getConfiguration()
    {
        return configuration;
    }

};


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Gamykla, kurianti objektus, sudaroancius mdoelio duomenu struktura.
 */
class ModelFactory
{
public:

    /// Konstruktorius.
    ModelFactory()
    {};

    /// Destruktorius.
    virtual ~ModelFactory()
    {};

    /// Kuria nauja srities duomenu struktura.
    virtual Area* newArea(
        cfg::Area*      configuration,
        PointFactory*   pointFactory,
        Dimension*      dimX,
        Dimension*      dimY
    ) = 0;

    /// Kuria nauja krasto duomenu struktura.
    virtual Bound* newBound(
        cfg::Bound*     configuration,
        PointFactory*   pointFactory,
        Dimension*      dim,
        Area*           prev,
        Area*           next
    ) = 0;

    /// Kuria nauja kampo duomenu struktura.
    virtual Corner* newCorner(
        PointFactory*   pointFactory,
        Bound*          top,
        Bound*          right,
        Bound*          bottom,
        Bound*          left
    ) = 0;

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
class Area
{
    friend class Bound;
protected:
    cfg::Area* configuration;
    Dimension *dimX;        ///< Dimensija pagal erdves X koordinate
    Dimension *dimY;        ///< Dimensija pagal erdves Y koordinate
    Bound     *boundTop;    ///< Srities krastas is virsaus.
    Bound     *boundRight;  ///< Srities krastas is desines.
    Bound     *boundBottom; ///< Srities krastas is apacios.
    Bound     *boundleft;   ///< Srities krastas is kaires.

public:
    Area(
        cfg::Area*  configuration,
        Dimension*  dimX,
        Dimension*  dimY
    )
    {
        this->configuration = configuration;
        this->dimX = dimX;
        this->dimY = dimY;
    }
    virtual ~Area()
    {}

    virtual int  moveTop() = 0;
    virtual int  moveRight() = 0;
    virtual int  moveBottom() = 0;
    virtual int  moveLeft() = 0;
    virtual void moveToColStart() = 0;
    virtual void moveToColEnd() = 0;
    virtual void moveToRowStart() = 0;
    virtual void moveToRowEnd() = 0;
    virtual Point* getTop() = 0;
    virtual Point* getRight() = 0;
    virtual Point* getBottom() = 0;
    virtual Point* getLeft() = 0;
    virtual Point* getCurrent() = 0;

    virtual cfg::Area* getConfiguration()
    {
        return configuration;
    }

};



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Sriciu krastai / sanduros, abstrakti realizacija.
 */
class Bound
{
    friend class Corner;
protected:
    cfg::Bound* configuration;
    Dimension*  dim;
    Area*       prev;
    Area*       next;

public:
    Bound(
        cfg::Bound* configuration,
        Dimension*  dim,
        Area*       prev,
        Area*       next
    )
    {
        this->configuration = configuration;
        this->dim = dim;
        this->prev = prev;
        this->next = next;
    }
    virtual ~Bound()
    {}

    virtual int  moveNext() = 0;
    virtual int  movePrev() = 0;
    virtual void moveToStart() = 0;
    virtual void moveToEnd() = 0;
    virtual Point* getNext() = 0;
    virtual Point* getPrev() = 0;
    virtual Point* getCurrent() = 0;

    virtual cfg::Bound* getConfiguration()
    {
        return configuration;
    }

};



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Kampo (kuriame susiduria keturios sritys) abstrakti realizacija.
 */
class Corner
{
protected:
    Bound *top;
    Bound *right;
    Bound *bottom;
    Bound *left;

public:
    Corner(
        Bound*          top,
        Bound*          right,
        Bound*          bottom,
        Bound*          left
    )
    {
        this->top    = top;
        this->right  = right;
        this->bottom = bottom;
        this->left   = left;
    }
    virtual ~Corner()
    {}

    virtual Point * getCurrent() = 0;

};



/* ************************************************************************** */
/* ************************************************************************** */
}  // namespace dm
#endif
