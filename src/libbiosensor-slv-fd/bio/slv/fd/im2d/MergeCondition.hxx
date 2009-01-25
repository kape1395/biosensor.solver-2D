#ifndef BIO_SLV_FD_IM2D_MergeCondition_HXX
#define BIO_SLV_FD_IM2D_MergeCondition_HXX
#include "../../../../biosensor-slv-fd.hxx"
#include "IBoundCondition.hxx"
#include "AreaSubSolver.hxx"
#include <log4cxx/logger.h>

BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *  Bound condition "Merge".
 *  NOTE: This condition works only for cartesiant coordinate system.
 *
 *  The step "through" is solves using implicit scheme and the step "along" is
 *  solved using explicit scheme. This is because the boundary condition is
 *  defined in one direction only. I thing this is subject to change.
 */
class MergeCondition : public IBoundCondition
{
private:
    log4cxx::LoggerPtr log;
    AreaSubSolver::EdgeData* edgePrev;
    AreaSubSolver::EdgeData* edgeNext;
    double diffusionPrev;
    double diffusionNext;
    int size;

    double a;   //!< Equation coefficient "a" (for the step "Through" and "Along").
    double b;   //!< Equation coefficient "b" (for the step "Through" and "Along").
    double c;   //!< Equation coefficient "c" (for the step "Through" and "Along").
    double f;   //!< Equation coefficient "f" (for the step "Through" only).

public:

    /**
     *  Constructor.
     *
     *  \param edgePrev         Reference to the data in the previous area.
     *  \param edgeNext         Reference to the data in the next area.
     *  \param diffusionPrev    Diffusion coefficient in the previous area.
     *  \param diffusionNext    Diffusion coefficient in the next area.
     */
    MergeCondition(
        AreaSubSolver::EdgeData* edgePrev,
        AreaSubSolver::EdgeData* edgeNext,
        double diffusionPrev,
        double diffusionNext
    );

    /**
     *
     */
    virtual ~MergeCondition();

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

};



BIO_SLV_FD_IM2D_NS_END

#endif
