#ifndef BIO_SLV_ISolverStateHolder_HXX
#define BIO_SLV_ISolverStateHolder_HXX
#include "../../biosensor.hxx"
#include "ISolverState.hxx"
BIO_SLV_NS_BEGIN


/**
 *  Something that can hold/store solver's state.
 */
class ISolverStateHolder
{
public:
    virtual ~ISolverStateHolder()
    {
        //  Empty virtual destructor.
    }

    /**
     *  Returns solver state.
     */
    virtual BIO_SLV_NS::ISolverState* getSolverState() = 0;

    /**
     *  Remembers state of the solver.
     */
    virtual void setSolverState(BIO_SLV_NS::ISolverState* state) = 0;

    /**
     *  Tells, if this holder has a state,
     */
    virtual bool hasSolverState() = 0;

};



BIO_SLV_NS_END
#endif
