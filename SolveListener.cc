#include "SolveListener.hh"
#include "sa/AbstractSA.hh"
#include "dm/AbstractDM.hh"
#include <ostream>
#include <cmath>


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Konstruktorius...
 *  \param out  I koki stream`a rasyti informacija.
 *  \param step Po kelinto zingsnio daryti output`a.
 */
sl::DebugSL::DebugSL(std::ostream &out, long step)
{
    this->stepToOutput  = step;
    this->out           = &out;
}

/**
 *  Destruktorius, tuscias.
 */
sl::DebugSL::~DebugSL()
{
    // Nothing
}

/**
 *  Dato output`a.
 */
void sl::DebugSL::solveEventOccured ( sa::Solver *solver )
{
    if (solver->getSolvedIterationCount() != stepToOutput)
        return;


    *out << "=================== Step " << solver->getSolvedIterationCount() << "============================\n";



    for ( int j = 0; j < solver->getData()->getPartsV(); j++ )
    {
        for ( int i = 0; i < solver->getData()->getPartsH(); i++ )
        {
            *out << "-------------- BEG OF AREA [" << i << ' ' << j << "]--------------------------\n";
            dm::Area *area = solver->getData()->getArea() [i][j];
            int substCount = solver->getData()->getConfiguration()->getSubstances().size();

            area->moveToColStart();
            do
            {
                area->moveToRowStart();
                do
                {
                    dm::Point *point = area->getCurrent();
                    *out << '(';
                    for ( int s = 0; s < substCount; s++ )
                    {
                        *out << ' ' << point->getSubstance ( s );
                    }
                    *out << " )\t";
                }
                while ( area->moveRight() >= 0 );
                *out << '\n';
            }
            while ( area->moveBottom() >= 0 );
            *out << "-------------- END OF AREA [" << i << ' ' << j << "]--------------------------\n\n";
        }
    }
}



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Konstruktorius.
 *  \param step Zingsnis, po kurio sustabdyti skaiciavima.
 */
sl::StopAtStepSL::StopAtStepSL(long step)
{
    this->maxStepCount = step;
}

/**
 *  Destruktorius.
 */
sl::StopAtStepSL::~StopAtStepSL()
{
    // Nothing
}

/**
 *  Jei jau isspresta tiek zingsniu, kiek nurodyta sitam
 *  listeneriui, tai sustabdyti skaiciavima.
 */
void sl::StopAtStepSL::solveEventOccured ( sa::Solver *solver )
{
    if (solver->getSolvedIterationCount() >= maxStepCount)
    {
        solver->stop();
    }
}



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Konstruktorius...
 *  \param out  I koki stream`a rasyti informacija.
 *  \param step Po kelinto zingsnio daryti output`a.
 */
sl::GnuplotDataSL::GnuplotDataSL(std::ostream &out, long step, cfg::Substance* substance)
{
    this->writeAfterStep = step;
    this->out            = &out;
    this->substance      = substance;
}

/**
 *  Destruktorius, tuscias.
 */
sl::GnuplotDataSL::~GnuplotDataSL()
{
    // Nothing
}

/**
 *  Dato output`a.
 */
void sl::GnuplotDataSL::solveEventOccured ( sa::Solver *solver )
{
    if (solver->getSolvedIterationCount() != writeAfterStep)
        return;


    int substanceIndex = solver->getData()->getSubstanceIndex(substance);


    bool lastAreaInRow  = false;
    bool lastAreaInCol  = false;
    bool lastRow        = false;
    bool lastCol        = false;
    bool firstCol       = false;

    (*out)
    << "# Substance=" << substance->getName()
    << " step=" << solver->getSolvedIterationCount()
    << " time=" << solver->getSolvedTime()
    << '\n';

    //  Pereinam per visas sritis (eilemis is virsaus i apacia).
    for ( int j = 0; j < solver->getData()->getPartsV(); j++ )
    {
        lastAreaInCol = (j >= solver->getData()->getPartsV() - 1);

        //  Kiekvienos srities, esancios naugrinejamoje juostoje,
        //  kursoriu nurodome i virsutine eilute.
        for ( int i = 0; i < solver->getData()->getPartsH(); i++ )
            solver->getData()->getArea()[i][j]->moveToColStart();

        //  Pereiname per visas juostos eilutes.
        do
        {
            //  Kiekvienoje juostos eiluteje einam per visas sritis
            //  toje juostoje.
            firstCol = true;
            for ( int i = 0; i < solver->getData()->getPartsH(); i++ )
            {
                lastAreaInRow = (i >= solver->getData()->getPartsH() - 1);
                dm::Area* area = solver->getData()->getArea()[i][j];

                //  Kiekvienoje toje srityje, pereinam per aktyvia eilute.
                area->moveToRowStart();
                do
                {
                    if (!firstCol)
                        (*out) << '\t';
                    (*out) << area->getCurrent()->getSubstance(substanceIndex);

                    lastCol = lastAreaInRow
                              ? area->moveRight() < 0
                              : area->moveRight() < 1;

                    firstCol = false;
                }
                while ( !lastCol );

                //  Pereinam prie sekancios eilutes.
                lastRow = lastAreaInCol
                          ? area->moveBottom() < 0
                          : area->moveBottom() < 1;
            }
            (*out) << '\n';
        }
        while ( !lastRow );
    }
    (*out) << '\n';
}



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Konstruktorius...
 *  \param stepInterval Kas kelias iteracijas tikrinti, ar substanciju
 *                      koncentracijose neatsirado NaN`u arba neigiamu skaiciu.
 */
sl::ErrorInDataListener::ErrorInDataListener(long stepInterval)
{
    this->stepInterval = stepInterval;
}

/**
 *  Destruktorius, tuscias.
 */
sl::ErrorInDataListener::~ErrorInDataListener()
{
    // Nothing
}

/**
 *  Tikrina duomenis (substanciju koncentracijas).
 */
void sl::ErrorInDataListener::solveEventOccured ( sa::Solver *solver )
{
    if ((solver->getSolvedIterationCount() % stepInterval) != 0)
        return;


    for ( int j = 0; j < solver->getData()->getPartsV(); j++ )
    {
        for ( int i = 0; i < solver->getData()->getPartsH(); i++ )
        {
            dm::Area *area = solver->getData()->getArea() [i][j];
            int substCount = solver->getData()->getConfiguration()->getSubstances().size();

            area->moveToColStart();
            do
            {
                area->moveToRowStart();
                do
                {
                    dm::Point *point = area->getCurrent();
                    for ( int s = 0; s < substCount; s++ )
                    {
                        double subst = point->getSubstance(s);
                        if (isnan(subst) || subst < 0.0)
                        {
                            std::cerr << "ERROR: Execution aborted, concentration NaN or <0 found.\n";
                            solver->stop();
                            return;
                        }
                    }
                }
                while ( area->moveRight() >= 0 );
            }
            while ( area->moveBottom() >= 0 );
        }
    }
}



/* ************************************************************************** */
/* ************************************************************************** */
