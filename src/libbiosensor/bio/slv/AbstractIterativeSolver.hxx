#ifndef BIO_SLV_ABSTRACTITERATIVESOLVER_HXX
#define BIO_SLV_ABSTRACTITERATIVESOLVER_HXX
#include "../../biosensor.hxx"
#include "AbstractSolver.hxx"
#include "IIterativeSolver.hxx"
#include <vector>
BIO_SLV_NS_BEGIN
class ISolverListener;


/**
 *  Abstract operations: solveIteration().
 */
class AbstractIterativeSolver : public AbstractSolver, public IIterativeSolver
{
private:
    double timeStep;
    double timeSolved;
    long   iterationsSolved;
    bool   stopped;
    std::vector<ISolverListener*> listeners;
    std::vector<ISolverListener*> listenersToDelete;

public:
    AbstractIterativeSolver(BIO_XML_NS::model::Model* config);
    virtual ~AbstractIterativeSolver();

    virtual void solve();
    virtual void stop();
    virtual bool isStopped();
    virtual double getTimeStep();
    virtual void setTimeStep(double timeStep);
    virtual long getSolvedIterationCount();
    virtual double getSolvedTime();
    virtual void addListener(ISolverListener* listener, bool deleteOnDestruction);

protected:
    virtual void invokeListeners();
    virtual void solveIteration() = 0;

};



BIO_SLV_NS_END

#endif
