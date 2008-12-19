#ifndef BIO_SLV_FD_IM2D_ConstantCondition_HXX
#define BIO_SLV_FD_IM2D_ConstantCondition_HXX
#include "../../../../biosensor-slv-fd.hxx"
#include "IBoundCondition.hxx"
#include <log4cxx/logger.h>

BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *
 */
class ConstantCondition : public IBoundCondition
{
public:

    /**
     *
     */
    ConstantCondition();

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
