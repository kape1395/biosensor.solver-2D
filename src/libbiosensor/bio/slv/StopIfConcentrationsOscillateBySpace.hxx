#ifndef BIO_SLV_StopIfConcentrationsOscillateBySpace_HXX
#define BIO_SLV_StopIfConcentrationsOscillateBySpace_HXX
#include "../../biosensor.hxx"
#include "../dm/IComposite2D.hxx"
#include "StopIfInvalidConcentrations.hxx"
#include "ISolverListener.hxx"

BIO_SLV_NS_BEGIN


/**
 *  Stops calculations, if oscillation by space is detected in the
 *  substance concenntrations.
 */
class StopIfConcentrationsOscillateBySpace : public BIO_SLV_NS::StopIfInvalidConcentrations
{

public:
    /**
     *  Constructor.
     */
    StopIfConcentrationsOscillateBySpace(
        ISolver* solver,
        long checkEveryNumberOfSteps = 100
    );

    /**
     *  Destructor.
     */
    virtual ~StopIfConcentrationsOscillateBySpace();

protected:

    /**
     *  Check one sub-area for the data validity.
     */
    virtual bool checkSubArea(BIO_DM_NS::IGrid2D* area, int areaPosH, int areaPosV);

};



BIO_SLV_NS_END
#endif
