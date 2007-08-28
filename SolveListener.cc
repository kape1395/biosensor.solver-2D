#include "SolveListener.hh"
#include "sa/AbstractSA.hh"
#include "dm/AbstractDM.hh"
#include <ostream>


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
