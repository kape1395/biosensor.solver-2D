#ifndef SA_BasicExplicit_HH
#define SA_BasicExplicit_HH
#include "Abstract.hh"
#include "../dm/Abstract.hh"

namespace sa
{
namespace basicexplicit
{



class Solver : public sa::Solver
{
public:
    Solver(dm::Model* data) : sa::Solver(data) {}
    virtual ~Solver() {}
    virtual void solve() {}
};



}   // namespace basicexplicit
}   // namespace sa

#endif
