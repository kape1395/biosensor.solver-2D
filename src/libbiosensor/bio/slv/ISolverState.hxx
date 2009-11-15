#ifndef BIO_SLV_ISolverState_HXX
#define BIO_SLV_ISolverState_HXX
#include "../../biosensor.hxx"
#include "../dm/IDataModel.hxx"
BIO_SLV_NS_BEGIN


/**
 *  Solver state interface.
 */
class ISolverState
{
public:
    virtual ~ISolverState()
    {
        //  Empty virtual destructor.
    }

    /**
     *  Returns current (simulation) time.
     */
    virtual double getTime() = 0;

    /**
     *  Returns iteration number.
     */
    virtual long getIteration() = 0;

    /**
     *  Returns data model.
     */
    virtual BIO_DM_NS::IDataModel* getData() = 0;
};



BIO_SLV_NS_END
#endif
