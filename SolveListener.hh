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
    long currentStep;
    long stepToOutput;
    std::ostream *out;

public:
    DebugSL(std::ostream &out, long step);
    virtual ~DebugSL();
    virtual void solveEventOccured(sa::Solver *solver);

};



/* ************************************************************************** */
/* ************************************************************************** */
}   // namespace sl
#endif
