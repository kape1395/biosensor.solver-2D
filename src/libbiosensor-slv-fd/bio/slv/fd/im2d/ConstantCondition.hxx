#ifndef BIO_SLV_FD_IM2D_ConstantCondition_HXX
#define BIO_SLV_FD_IM2D_ConstantCondition_HXX
#include "../../../../biosensor-slv-fd.hxx"
#include "IBoundCondition.hxx"
#include "AreaSubSolver.hxx"
#include <log4cxx/logger.h>

BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *  Bound condition "Constant".
 */
class ConstantCondition : public IBoundCondition
{
private:
    AreaSubSolver::EdgeData* edge;
    double concentration;
    bool atStart;

public:

    /**
     *  Constructor.
     *
     *  \param edge Reference to the data in the area.
     *  \param concentration    Concentration to be constant.
     */
    ConstantCondition(
        AreaSubSolver::EdgeData* edge,
        double concentration,
        bool atStart
    );

    /**
     *
     */
    virtual ~ConstantCondition();

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
