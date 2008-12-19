#ifndef BIO_SLV_FD_IM2D_IBoundCondition_HXX
#define BIO_SLV_FD_IM2D_IBoundCondition_HXX
#include "../../../../biosensor-slv-fd.hxx"
#include <log4cxx/logger.h>

BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *
 */
class IBoundCondition
{
public:

    /**
     *
     */
    virtual ~IBoundCondition()
    {
        // Nothing.
    };

    /**
     *
     */
    virtual void solveThroughForward() = 0;

    /**
     *
     */
    virtual void solveThroughBackward() = 0;

    /**
     *
     */
    virtual void solveAlongForward() = 0;

    /**
     *
     */
    virtual void solveAlongBackward() = 0;

};



BIO_SLV_FD_IM2D_NS_END

#endif
