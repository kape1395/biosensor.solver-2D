/// \file
/// Valdymo posisteme.
///
#ifndef SolveListener_HH
#define SolveListener_HH
#include "sa/AbstractSA.hh"
#include <ostream>


namespace sl
{
/* ************************************************************************** */
/* ************************************************************************** */



/**
 *  Sprendejas, kas iteracija raso viska i STDOUT.
 *
 */
class DebugSL : public sa::SolveListener
{
protected:
    long stepToOutput;
    std::ostream *out;

public:
    DebugSL(std::ostream &out, long step);
    virtual ~DebugSL();
    virtual void solveEventOccured(sa::Solver *solver);

};



/**
 *  Sustabdo sprendima, jei pasiekiama nurodyta iteracija.
 */
class StopAtStepSL : public sa::SolveListener
{
protected:
    long maxStepCount;

public:
    StopAtStepSL(long step);
    virtual ~StopAtStepSL();
    virtual void solveEventOccured(sa::Solver *solver);

};


/**
 *  Raso duomenu faila, tinkama atvaizduoti gnuplot`u.
 */
class GnuplotDataSL : public sa::SolveListener
{
protected:
    long            writeAfterStep;
    cfg::Substance* substance;
    std::ostream*   out;

public:
    GnuplotDataSL(std::ostream &out, long step, cfg::Substance* substance);
    virtual ~GnuplotDataSL();
    virtual void solveEventOccured(sa::Solver *solver);

};


/**
 *  Tikrina, ar neatsirado NaN`u arba neigiamu skaiciu substratu
 *  koncentracijose.
 */
class ErrorInDataListener : public sa::SolveListener
{
protected:
    long stepInterval;  ///< Kas kiek zingsniu tikrinti.

public:
    ErrorInDataListener(long stepInterval);
    virtual ~ErrorInDataListener();
    virtual void solveEventOccured(sa::Solver *solver);

};


/* ************************************************************************** */
/* ************************************************************************** */
}   // namespace sl
#endif
