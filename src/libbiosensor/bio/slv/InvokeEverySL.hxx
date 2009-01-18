#ifndef BIO_SLV_InvokeEverySL_HXX
#define BIO_SLV_InvokeEverySL_HXX
#include "../../biosensor.hxx"
#include "ISolverListener.hxx"
#include <log4cxx/logger.h>
#include <vector>
BIO_SLV_NS_BEGIN


/**
 *
 */
class InvokeEverySL : public ISolverListener
{
private:
    log4cxx::LoggerPtr log;
    IIterativeSolver* solver;
    long stepCount;
    long nextStop;
    std::vector<ISolverListener*> listeners;

public:
    /**
     *  Constructor.
     */
    InvokeEverySL(
        ISolver* solver,
        long stepCount
    );

    /**
     *  Destructor.
     */
    virtual ~InvokeEverySL();

    /**
     *  EventListener.
     */
    virtual void solveEventOccured();

    /**
     *  Add nested listener.
     */
    virtual void addListener(ISolverListener* listener);

};



BIO_SLV_NS_END
#endif
