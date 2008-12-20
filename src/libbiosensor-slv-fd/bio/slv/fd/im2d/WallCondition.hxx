#ifndef BIO_SLV_FD_IM2D_WallCondition_HXX
#define BIO_SLV_FD_IM2D_WallCondition_HXX
#include "../../../../biosensor-slv-fd.hxx"
#include "IBoundCondition.hxx"
#include "AreaSubSolver.hxx"
#include <log4cxx/logger.h>

BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *
 */
class WallCondition : public IBoundCondition
{
private:
    AreaSubSolver::EdgeData* edge;
    bool atStart;

public:

    /**
     *  Constructor.
     *
     *  \param edge     Reference to the data in the area.
     *  \param atStart  Is this condition at top|left (true) or bottom|right (false)?
     */
    WallCondition(
        AreaSubSolver::EdgeData* edge,
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

};



BIO_SLV_FD_IM2D_NS_END

#endif
