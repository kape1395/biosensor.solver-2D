#include "StopIfInvalidConcentrations.hxx"
#include "../Exception.hxx"
#include <iostream>
#include <cmath>
#include <memory>
#include "../Logging.hxx"
#define LOGGER "libbiosensor::StopIfInvalidConcentrations: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::StopIfInvalidConcentrations::StopIfInvalidConcentrations(
    ISolver* solver,
    long checkEveryNumberOfSteps
)
{
    this->iterativeSolver = dynamic_cast<BIO_SLV_NS::IIterativeSolver*>(solver);
    this->dataModel = dynamic_cast<BIO_DM_NS::IComposite2D*>(solver->getData());
    this->checkEveryNumberOfSteps = checkEveryNumberOfSteps;

    if (this->checkEveryNumberOfSteps <= 0)
        this->checkEveryNumberOfSteps = 1;

    if (iterativeSolver == 0)
        throw Exception("StopIfInvalidConcentrations: Solver must implement IIterativeSolver");

    if (dataModel == 0)
        throw Exception("StopIfInvalidConcentrations: Solver dataModel must implement IComposite2D");

    this->nextStepForCheck = checkEveryNumberOfSteps;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::StopIfInvalidConcentrations::~StopIfInvalidConcentrations()
{
    //  Nothing to do here
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::StopIfInvalidConcentrations::solveEventOccured()
{
    if (iterativeSolver->getSolvedIterationCount() < nextStepForCheck)
    {
        return;
    }

    for (int v = 0; v < dataModel->sizeV(); v++)
    {
        for (int h = 0; h < dataModel->sizeH(); h++)
        {
            if (!checkSubArea(dataModel->getArea(h, v)))
            {
                iterativeSolver->stop(false);
                LOG_ERROR(LOGGER << "The solver is stopped because it is in an invalid state."
                          << " Negative concentration of a substance or NaN was"
                          << " found in the subArea with position:"
                          << " h=" << h << " v=" << v
                         );
            }
        }
    }
    LOG_INFO(LOGGER << "Validation successful");

    nextStepForCheck = iterativeSolver->getSolvedIterationCount() + checkEveryNumberOfSteps;
}


/* ************************************************************************** */
/* ************************************************************************** */
bool BIO_SLV_NS::StopIfInvalidConcentrations::checkSubArea(BIO_DM_NS::IGrid2D* area)
{
    std::auto_ptr<BIO_DM_NS::ICursor2D> cursor(area->newGridCursor());

    for (cursor->colStart(); cursor->rowStart(), cursor->isValid(); cursor->down())
    {
        for ( ; cursor->isValid(); cursor->right())
        {
            BIO_DM_NS::IConcentrations* concentrations = cursor->getConcentrations();
            for (int s = 0; s < area->getSubstanceCount(); s++)
            {
                double c = concentrations->getConcentration(s);
                if (area->getSubstanceConf(s) && (std::isnan(c) || c < 0))
                {
                    LOG_WARN(LOGGER << "An invalid concentration value " << c
                             << " found for the substance "
                             << area->getSubstanceConf(s)->name()
                            );
                    return false;
                }
            }
        }
    }

    return true;
}


/* ************************************************************************** */
/* ************************************************************************** */
