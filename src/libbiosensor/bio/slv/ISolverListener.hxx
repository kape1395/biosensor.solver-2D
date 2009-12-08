#ifndef BIO_SLV_ISolverListener_HXX
#define BIO_SLV_ISolverListener_HXX
#include "../../biosensor.hxx"
#include "IIterativeSolver.hxx"
BIO_SLV_NS_BEGIN


/**
 *
 */
class ISolverListener
{
public:
    virtual ~ISolverListener()
    {
        //  Empty virtual destructor.
    }

    virtual void solveEventOccured() = 0;
    virtual void reset() = 0;
};



BIO_SLV_NS_END
#endif
