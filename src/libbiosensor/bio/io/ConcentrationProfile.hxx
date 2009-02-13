#ifndef BIO_IO_ConcentrationProfile_HXX
#define BIO_IO_ConcentrationProfile_HXX
#include "../../biosensor.hxx"
#include "../dm/IGrid2D.hxx"
#include "../dm/ICursor2D.hxx"
#include "IOutput.hxx"
#include "IContext.hxx"
#include "IRepeatable.hxx"
#include <string>
BIO_IO_NS_BEGIN


/**
 *
 */
class ConcentrationProfile : public BIO_IO_NS::IOutput, public BIO_IO_NS::IRepeatable
{
private:

    std::string name;
    BIO_SLV_NS::ISolver* solver;
    BIO_IO_NS::IContext* Context;

    BIO_DM_NS::IGrid2D* grid;
    BIO_DM_NS::ICursor2D* cursor;

    bool indexed;
    long currentIndex;

public:
    /**
     *  Constructor.
     */
    ConcentrationProfile(
        std::string& name,
        BIO_SLV_NS::ISolver* solver,
        BIO_IO_NS::IContext* Context
    );

    /**
     *  Destructor.
     */
    virtual ~ConcentrationProfile();

    /**
     *  EventListener.
     */
    virtual void solveEventOccured();

    /**
     *  ...
     */
    virtual void setRepeatable(bool repeatable)
    {
        this->indexed = repeatable;
    }

};



BIO_IO_NS_END
#endif
