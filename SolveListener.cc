#include "SolveListener.hh"
#include "sa/AbstractSA.hh"
#include "dm/AbstractDM.hh"
#include <ostream>


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *
 */
sl::DebugSL::DebugSL(std::ostream &out, long step)
{
    this->currentStep   = -1;
    this->stepToOutput  = step;
    this->out           = &out;
}



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *
 */
sl::DebugSL::~DebugSL()
{
    // Nothing
}



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *
 */
void sl::DebugSL::solveEventOccured ( sa::Solver *solver )
{
    if (++currentStep != stepToOutput)
        return;


    *out << "=================== Step " << currentStep << "============================\n";



    for ( int j = 0; j < solver->getData()->getPartsV(); j++ )
    {
        for ( int i = 0; i < solver->getData()->getPartsH(); i++ )
        {
            *out << "-------------- BEG OF AREA [" << i << ' ' << j << "]--------------------------\n";
            dm::Area *area = solver->getData()->getArea() [i][j];
            //t substCount = area->getConfiguration()->getMedium()->getDiffusions().size();
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
