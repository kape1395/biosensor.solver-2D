#ifndef BIO_IO_ConcentrationProfile_HXX
#define BIO_IO_ConcentrationProfile_HXX
#include "../../biosensor.hxx"
#include "../dm/IGrid2D.hxx"
#include "../dm/ICursor2D.hxx"
#include "IOutput.hxx"
#include "IOutputContext.hxx"
#include <log4cxx/logger.h>
#include <string>
BIO_IO_NS_BEGIN


/**
 *
 */
class ConcentrationProfile : public BIO_IO_NS::IOutput
{
private:
    log4cxx::LoggerPtr log;

    std::string name;
    BIO_SLV_NS::ISolver* solver;
    BIO_IO_NS::IOutputContext* outputContext;

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
        long indexed,
        BIO_SLV_NS::ISolver* solver,
        BIO_IO_NS::IOutputContext* outputContext
    );

    /**
     *  Destructor.
     */
    virtual ~ConcentrationProfile();

    /**
     *  EventListener.
     */
    virtual void solveEventOccured();

};



BIO_IO_NS_END
#endif
