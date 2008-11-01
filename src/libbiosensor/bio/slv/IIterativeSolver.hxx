#ifndef BIO_SLV_IIterativeSolver_HXX
#define BIO_SLV_IIterativeSolver_HXX
#include "../../biosensor.hxx"
#include "ISolver.hxx"
BIO_SLV_NS_BEGIN
class ISolverListener;


/**
 *
 */
class IIterativeSolver //: public ISolver -- to avoid diamond inheritance
{
public:
    virtual void stop() = 0;
    virtual bool isStopped() = 0;
    virtual double getTimeStep() = 0;
    virtual void setTimeStep(double timeStep) = 0;
    virtual long getSolvedIterationCount() = 0;
    virtual double getSolvedTime() = 0;
    virtual void addListener(ISolverListener* listener) = 0;
};



BIO_SLV_NS_END
#endif
