#ifndef BIO_SLV_FD_IM2D_WallCondition_HXX
#define BIO_SLV_FD_IM2D_WallCondition_HXX
#include "../../../../biosensor-slv-fd.hxx"
#include "IBoundCondition.hxx"
#include "AreaSubSolver.hxx"

BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *  Bound condition "Wall" (or "Non-leakage").
 */
class WallCondition : public IBoundCondition
{
private:

    IAreaEdgeData* edge;
    bool atStart;

public:

    /**
     *  Constructor.
     *
     *  \param edge     Reference to the data in the area.
     *  \param atStart  Is this condition at top|left (true) or bottom|right (false)?
     */
    WallCondition(
        IAreaEdgeData* edge,
        bool atStart
    );

    /**
     *
     */
    virtual ~WallCondition();

    /**
     *
     */
    virtual void solveThroughForward();

    /**
     *
     */
    virtual void solveThroughBackward();

    /**
     *
     */
    virtual void solveAlongForward();

    /**
     *
     */
    virtual void solveAlongBackward();

    /**
     *
     */
    virtual void applyInitialValues();

    /**
     *
     */
    virtual double getConcentration(int x);

    /**
     *  Set concentration for the substance at specified point.
     *
     *  \param x    Point index;
     *  \param c    New concentration.
     */
    virtual void setConcentration(int x, double c);

};



BIO_SLV_FD_IM2D_NS_END

#endif
