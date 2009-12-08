#ifndef BIO_IO_CurrentDensity_HXX
#define BIO_IO_CurrentDensity_HXX
#include "../../biosensor.hxx"
#include "../slv/ISolver.hxx"
#include "IOutput.hxx"
#include "IContext.hxx"
#include <string>
#include <ostream>
BIO_IO_NS_BEGIN


/**
 *
 */
class CurrentDensity : public BIO_IO_NS::IOutput
{
private:

    std::string name;
    BIO_SLV_NS::ISolver* solver;
    BIO_IO_NS::IContext* Context;

    std::ostream* output;

public:
    /**
     *  Constructor.
     */
    CurrentDensity(
        std::string& name,
        BIO_SLV_NS::ISolver* solver,
        BIO_IO_NS::IContext* Context
    );

    /**
     *  Destructor.
     */
    virtual ~CurrentDensity();

    /**
     *  EventListener.
     */
    virtual void solveEventOccured();

    /**
     *  Reset listener's internal state.
     */
    virtual void reset()
    {
        //  Nothing to reset.
    }

};



BIO_IO_NS_END
#endif
