#ifndef SA_BasicExplicit_HH
#define SA_BasicExplicit_HH
#include "Abstract.hh"
#include "../dm/Abstract.hh"

namespace sa
{
namespace basicexplicit
{

// TODO: Cia padaryti isreikstine schema.
// TODO: I Model strukturas ideti biojutiklio parametrus.


class Solver : public sa::Solver
{
public:
    Solver(dm::Model* data) : sa::Solver(data)
    {}
    virtual ~Solver()
    {}
    virtual void solve()
    {}
};



}   // namespace basicexplicit
}   // namespace sa

#endif
