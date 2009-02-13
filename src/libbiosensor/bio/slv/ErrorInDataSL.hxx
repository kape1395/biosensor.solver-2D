#ifndef BIO_SLV_ErrorInDataSL_HXX
#define BIO_SLV_ErrorInDataSL_HXX
#include "../../biosensor.hxx"
#include "ISolverListener.hxx"
BIO_SLV_NS_BEGIN


/**
 *
 */
class ErrorInDataSL : public ISolverListener
{
public:
    /**
     *  Constructor.
     */
    ErrorInDataSL();

    /**
     *  Destructor.
     */
    virtual ~ErrorInDataSL();

    /**
     *  EventListener.
     */
    virtual void solveEventOccured();

};



BIO_SLV_NS_END
#endif
