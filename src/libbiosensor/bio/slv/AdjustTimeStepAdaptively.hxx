#ifndef BIO_SLV_AdjustTimeStepAdaptively_HXX
#define BIO_SLV_AdjustTimeStepAdaptively_HXX
#include "../../biosensor.hxx"
#include "ISolverListener.hxx"
#include "AdjustTimeStepByFactor.hxx"
#include "ISolverListener.hxx"
#include "../io/ConcentrationProfile.hxx"
#include <vector>
BIO_SLV_NS_BEGIN


/**
 *  Adaptive time step adjuster.
 */
class AdjustTimeStepAdaptively : public AdjustTimeStepByFactor
{
private:
    std::vector<ISolverListener*> stopConditions;
    BIO_IO_NS::ConcentrationProfile* concentrationWriter;
    long increateAfterStep;

public:

    /**
     *  Constructor.
     */
    AdjustTimeStepAdaptively(
        ISolver* solver,
        double factor,
        long adjustEveryNumberOfSteps,
        double maxTimeStep,
        BIO_IO_NS::ConcentrationProfile* concentrationWriter,
        std::vector<ISolverListener*>& stopConditions
    );

    /**
     *  Destructor.
     */
    virtual ~AdjustTimeStepAdaptively();

protected:
    /**
     *  Calculates new time step.
     */
    virtual double getNewTimeStep();

};



BIO_SLV_NS_END
#endif
