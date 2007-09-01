#ifndef DM_AbstractDM_HH
#define DM_AbstractDM_HH
#include <list>
#include <iostream>
#include "../cfg/Config.hh"
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
class Dimension;
class ConstantDimension;




/**
 *  Krypciu enumeracija.
 */
enum Direction
{
    HORIZONTAL = 0,
    VERTICAL   = 1
};




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
    //typedef std::list<Dimension*> DimensionList;
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

    virtual Dimension* createDimensionByConfig(
        cfg::DimensionPart* config,
        Direction direction,
        double startPosition
    );

public:
    /// Kosntruktorius.
    Model(
        cfg::Config*    configuration,
        PointFactory*   pointFactory,
        ModelFactory*   modelFactory
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

    virtual int getSubstanceIndex(cfg::Substance* substance);
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
    void*      solver;
    Dimension *dimX;        ///< Dimensija pagal erdves X koordinate
    Dimension *dimY;        ///< Dimensija pagal erdves Y koordinate
    Bound     *boundTop;    ///< Srities krastas is virsaus.
    Bound     *boundRight;  ///< Srities krastas is desines.
    Bound     *boundBottom; ///< Srities krastas is apacios.
    Bound     *boundLeft;   ///< Srities krastas is kaires.

public:
    Area(
        cfg::Area*  configuration,
        Dimension*  dimX,
        Dimension*  dimY
    );
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

    virtual void* getSolver()
    {
        return solver;
    }

    virtual void setSolver(void* solver)
    {
        this->solver = solver;
    }

    virtual Dimension* getDimensionX()
    {
        return dimX;
    }

    virtual Dimension* getDimensionY()
    {
        return dimY;
    }

    virtual Bound* getBoundTop()
    {
        return boundTop;
    }

    virtual Bound* getBoundRight()
    {
        return boundRight;
    }

    virtual Bound* getBoundBottom()
    {
        return boundBottom;
    }

    virtual Bound* getBoundLeft()
    {
        return boundLeft;
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
    void*       solver;
    Dimension*  dim;
    Area*       prevArea;
    Area*       nextArea;
    Corner*     prevCorner;
    Corner*     nextCorner;
public:
    Bound(
        cfg::Bound* configuration,
        Dimension*  dim,
        Area*       prevArea,
        Area*       nextArea
    );
    virtual ~Bound()
    {}

    virtual int  moveNext() = 0;
    virtual int  movePrev() = 0;
    virtual void moveToStart() = 0;
    virtual void moveToEnd() = 0;
    virtual Point* getNext() = 0;
    virtual Point* getPrev() = 0;
    virtual Point* getCurrent() = 0;

    virtual Area* getNextArea()
    {
        return nextArea;
    }

    virtual Area* getPrevArea()
    {
        return prevArea;
    }

    virtual Point* getNextAreaPoint() = 0;  ///< Grazina atitnkama antra nuo krasto taska.
    virtual Point* getPrevAreaPoint() = 0;  ///< Grazina atitnkama antra nuo krasto taska.

    virtual Dimension* getDimension()
    {
        return dim;
    }

    virtual cfg::Bound* getConfiguration()
    {
        return configuration;
    }

    virtual void* getSolver()
    {
        return solver;
    }

    virtual void setSolver(void* solver)
    {
        this->solver = solver;
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
    Bound* top;
    Bound* right;
    Bound* bottom;
    Bound* left;
    void*  solver;

public:
    Corner(
        Bound* top,
        Bound* right,
        Bound* bottom,
        Bound* left
    );
    virtual ~Corner()
    {}

    virtual Point * getCurrent() = 0;

    virtual void* getSolver()
    {
        return solver;
    }

    virtual void setSolver(void* solver)
    {
        this->solver = solver;
    }

    virtual Bound* getTopBound()
    {
        return top;
    }

    virtual Bound* getRightBound()
    {
        return right;
    }

    virtual Bound* getBottomBound()
    {
        return bottom;
    }

    virtual Bound* getLeftBound()
    {
        return left;
    }
};



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Dimensijos atkarpa.
 */
class Dimension
{
protected:
    Direction direction;   ///< 0 - horizontali, 1 - vertikali.
    double    length;      ///< Dimensijos atkarpos ilgis.
    double    start;       ///< Dimensijos pradzia.

public:
    /// Konstruktorius.
    Dimension(
        Direction   direction,
        double      start,
        double      length
    )
    {
        this->direction = direction;
        this->start     = start;
        this->length    = length;
    }

    /// Destruktorius.
    virtual ~Dimension()
    {}

    virtual int      getPointCount() = 0;
    virtual double   getLength()
    {
        return length;
    }
    virtual Direction getDirection()
    {
        return direction;
    }
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
        Direction   direction,
        double      start,
        double      length,
        int         steps
    );

    virtual ~ConstantDimension();

    virtual int getPointCount()
    {
        return steps + 1;
    }

    virtual double* getPositions()
    {
        return positions;
    }

    virtual double* getIntervals()
    {
        return intervals;
    }

};



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Iteratorius per visa duomenu struktura.
 *  TODO: Gal cia is ties bus geresnis sprendimas???
class Iterator
{
protected:
    Model* model;
 
 
    Iterator(Model* model)
    {
        this->model = model;
    }
 
 
public:
    virtual ~Iterator() {}
    virtual Iterator* top() = 0;
    virtual Iterator* bottom() = 0;
    virtual Iterator* right() = 0;
    virtual Iterator* left() = 0;
    virtual Point* operator *() = 0;
};
*/


/* ************************************************************************** */
/* ************************************************************************** */
}  // namespace dm
#endif
