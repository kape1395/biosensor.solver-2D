#ifndef BIO_IO_ConcentrationProfile_HXX
#define BIO_IO_ConcentrationProfile_HXX
#include "../../biosensor.hxx"
#include "../dm/IGrid2D.hxx"
#include "../dm/ICursor2D.hxx"
#include "IOutput.hxx"
#include "IContext.hxx"
#include "IRepeatable.hxx"
#include "ConcentrationProfileReader.hxx"
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
    BIO_IO_NS::IContext* context;

    BIO_DM_NS::IGrid2D* grid;
    BIO_DM_NS::ICursor2D* cursor;

    bool indexed;
    bool haveLastOutput;
    long currentIndex;

public:
    /**
     *  Constructor.
     */
    ConcentrationProfile(
        std::string& name,
        BIO_SLV_NS::ISolver* solver,
        BIO_IO_NS::IContext* context
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
     *  Reset listener's internal state.
     */
    virtual void reset();

    /**
     *  Set true, if this output writer will be called multiple times.
     */
    virtual void setRepeatable(bool repeatable);

    /**
     *  Returns reader, that is configured to read last concentration file,
     *  produced by this writer.
     *  Returned object should be deleted by the caller.
     */
    virtual BIO_IO_NS::ConcentrationProfileReader* createReaderForLastOutput();

};



BIO_IO_NS_END
#endif
