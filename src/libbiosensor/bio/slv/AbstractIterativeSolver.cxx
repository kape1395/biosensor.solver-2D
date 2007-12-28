#include "AbstractIterativeSolver.hxx"
#include "ISolverListener.hxx"
BIO_SLV_NS_BEGIN


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Constructor.
 */
AbstractIterativeSolver::AbstractIterativeSolver(
BIO_XML_NS::model::Model* config
) : AbstractSolver(config)
{
    timeStep = 0;
    timeSolved = 0;
    iterationsSolved = 0;
    stopped = true;
    listeners.clear();
}


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Destructor.
 */
AbstractIterativeSolver::~AbstractIterativeSolver()
{
    // nothing
}


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Solve model.
 *  TODO: Can it be invoked after stop?
 */
void AbstractIterativeSolver::solve()
{
    stopped = false;
    invokeListeners();
    while (!stopped)
    {
        solveIteration();
        iterationsSolved++;
        timeSolved += timeStep;
        invokeListeners();
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Stop the solver.
 */
void AbstractIterativeSolver::stop()
{
    stopped = true;
    invokeListeners();
}


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Is solved stopped?
 */
bool AbstractIterativeSolver::isStopped()
{
    return this->stopped;
}


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Return time step.
 */
double AbstractIterativeSolver::getTimeStep()
{
    return this->timeStep;
}


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Set new time step.
 */
void AbstractIterativeSolver::setTimeStep(double timeStep)
{
    this->timeStep = timeStep;
}


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Return solved iterations count.
 */
long AbstractIterativeSolver::getSolvedIterationCount()
{
    return this->iterationsSolved;
}


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Return solved time.
 */
double AbstractIterativeSolver::getSolvedTime()
{
    return this->timeSolved;
}


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Register new listener.
 */
void AbstractIterativeSolver::addListener(ISolverListener* listener)
{
    this->listeners.push_back(listener);
}


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Invoke all registered listeners.
 */
void AbstractIterativeSolver::invokeListeners()
{
    for (int i = 0; i < listeners.size(); i++)
    {
        listeners[i]->solveEventOccured(this);
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS_END
