#include "StopIfSumOfConcentrationsNonConst.hxx"
#include "../Exception.hxx"
#include "../cfg/StructureAnalyzer.hxx"
#include "../dm/Cursor2DWithoutBounds.hxx"
#include <iostream>
#include <cmath>
#include <memory>
#include "../Logging.hxx"
#define LOGGER "libbiosensor::StopIfSumOfConcentrationsNonConst: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::StopIfSumOfConcentrationsNonConst::StopIfSumOfConcentrationsNonConst(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::MediumName& mediumName,
    BIO_XML_MODEL_NS::SymbolName& constant,
    double error,
    std::vector<BIO_XML_MODEL_NS::SubstanceName*> substances,
    long checkEveryNumberOfSteps
)
{
    this->mediumName = mediumName;
    this->iterativeSolver = dynamic_cast<BIO_SLV_NS::IIterativeSolver*>(solver);
    this->error = error;
    this->checkEveryNumberOfSteps = checkEveryNumberOfSteps;

    if (this->checkEveryNumberOfSteps <= 0)
        this->checkEveryNumberOfSteps = 1;


    if (iterativeSolver == 0)
        throw Exception("StopIfSumOfConcentrationsNonConst: Solver must implement IIterativeSolver");

    BIO_DM_NS::IComposite2D* dataModel = dynamic_cast<BIO_DM_NS::IComposite2D*>(solver->getData());
    if (dataModel == 0)
        throw Exception("StopIfSumOfConcentrationsNonConst: Solver dataModel must implement IComposite2D");


    std::auto_ptr<BIO_CFG_NS::StructureAnalyzer> structAnalyzer(new BIO_CFG_NS::StructureAnalyzer(solver->getConfig()));


    this->constant = structAnalyzer->getSymbol(constant)->value();


    //
    //  Resolve substance indexes
    //
    for (std::vector<BIO_XML_MODEL_NS::SubstanceName*>::iterator subst = substances.begin(); subst < substances.end(); subst++)
    {
        substanceIncexes.push_back(structAnalyzer->getSubstanceIndex(**subst));
    }


    //
    //  Get all needed areas.
    //
    for (int h = 0; h < dataModel->sizeH(); h++)
    {
        for (int v = 0; v < dataModel->sizeV(); v++)
        {
            if (structAnalyzer->getMediumName(h, v) && (structAnalyzer->getMediumName(h, v)->compare(mediumName) == 0))
            {
                BIO_DM_NS::IGrid2D* area = dataModel->getArea(h, v);
                areas.push_back(area);

                //
                //  Check if all substances are defined in the specified medium.
                //
                std::vector<int> localSubstIndexes = structAnalyzer->getSubstanceIndexesInArea(h, v);
                for (unsigned s = 0; s < substanceIncexes.size(); s++)
                {
                    bool substanceFound = false;
                    for (unsigned i = 0; i < localSubstIndexes.size(); i++)
                    {
                        if (localSubstIndexes[i] == substanceIncexes[s])
                        {
                            substanceFound = true;
                            break;
                        }
                    }
                    if (!substanceFound)
                    {
                        throw Exception("StopIfSumOfConcentrationsNonConst: substance not exists in the specified medium.");
                    }
                }


            } // if name
        }
    }
    if (areas.size() == 0)
    {
        throw Exception("StopIfSumOfConcentrationsNonConst: No areas were found with specified medium name.");
    }

    reset();
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::StopIfSumOfConcentrationsNonConst::~StopIfSumOfConcentrationsNonConst()
{
    //  Nothing to do here
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::StopIfSumOfConcentrationsNonConst::solveEventOccured()
{
    if (iterativeSolver->getSolvedIterationCount() < nextStepForCheck)
    {
        return;
    }

    bool success = true;

    for (std::vector<BIO_DM_NS::IGrid2D*>::iterator area = areas.begin(); area < areas.end(); area++)
    {
        if (!checkSubArea(*area))
        {
            iterativeSolver->stop(false);
            LOG_ERROR(LOGGER
                      << "The solver is stopped because it is in an invalid state."
                      << " Sum of the substance concentrations is not equal to the specified constant"
                      << " in the medium " << mediumName
                     );
            success = false;
            break;
        }
    }
    if (success)
    {
        LOG_DEBUG(LOGGER << "Validation for medium " << mediumName << " successful");
    }

    reset();
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::StopIfSumOfConcentrationsNonConst::reset()
{
    nextStepForCheck = iterativeSolver->getSolvedIterationCount() + checkEveryNumberOfSteps;
}


/* ************************************************************************** */
/* ************************************************************************** */
bool BIO_SLV_NS::StopIfSumOfConcentrationsNonConst::checkSubArea(BIO_DM_NS::IGrid2D* area)
{
    //
    //  Validation is now performed not including boundary points of the area?
    //

    std::auto_ptr<BIO_DM_NS::ICursor2D> baseCursor(area->newGridCursor());
    BIO_DM_NS::Cursor2DWithoutBounds cursor(*baseCursor);

    for (cursor.colStart(); cursor.rowStart(), cursor.isValid(); cursor.down())
    {
        for ( ; cursor.isValid(); cursor.right())
        {
            BIO_DM_NS::IConcentrations* concentrations = cursor.getConcentrations();
            double sum = 0.0;
            for (unsigned i = 0; i < substanceIncexes.size(); i++)
            {
                sum += concentrations->getConcentration(substanceIncexes[i]);
            }

            if ((std::abs(constant - sum) / constant) > error)
            {
                LOG_WARN(LOGGER << "Validation failed: constant=" << constant << " sum=" << sum << " permitedError=" << error);
                return false;
            }
        }
    }
    return true;
}


/* ************************************************************************** */
/* ************************************************************************** */
