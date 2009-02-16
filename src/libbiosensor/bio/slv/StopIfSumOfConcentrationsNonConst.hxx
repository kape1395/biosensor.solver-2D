#ifndef BIO_SLV_StopIfSumOfConcentrationsNonConst_HXX
#define BIO_SLV_StopIfSumOfConcentrationsNonConst_HXX
#include "../../biosensor.hxx"
#include "ISolver.hxx"
#include "ISolverListener.hxx"
#include "../dm/IComposite2D.hxx"
#include <biosensor-xml.hxx>
#include <string>
#include <vector>

BIO_SLV_NS_BEGIN


/**
 *  Stop condition.
 */
class StopIfSumOfConcentrationsNonConst : public ISolverListener
{
private:

    BIO_SLV_NS::IIterativeSolver* iterativeSolver;
    double constant;
    double error;
    std::vector<int> substanceIncexes;
    std::vector<BIO_DM_NS::IGrid2D*> areas;

    long checkEveryNumberOfSteps;
    long nextStepForCheck;

public:

    /**
     *  Constructor.
     *
     *  \param solver       Target solver.
     *  \param mediumName   Medium name, for which this stop condition is applied.
     *  \param constant     Constant, to which sum of the substances must be equal.
     *  \param error        Allowed error (relatime error).
     *  \param substances   Substances, concentrations of which is to be summarized.
     *  \param checkEveryNumberOfSteps  Number of steps, ...
     */
    StopIfSumOfConcentrationsNonConst(
        BIO_SLV_NS::ISolver*            solver,
        BIO_XML_MODEL_NS::MediumName&   mediumName,
        BIO_XML_MODEL_NS::SymbolName&   constant,
        double                          error,
        std::vector<BIO_XML_MODEL_NS::SubstanceName*> substances,
        long checkEveryNumberOfSteps = 100
    );

    /**
     *  Destructor.
     */
    virtual ~StopIfSumOfConcentrationsNonConst();

    /**
     *  EventListener.
     */
    virtual void solveEventOccured();

private:

    /**
     *  Check one sub-area.
     */
    bool checkSubArea(BIO_DM_NS::IGrid2D* area);

};



BIO_SLV_NS_END
#endif
