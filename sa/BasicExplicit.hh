#ifndef SA_BasicExplicit_HH
#define SA_BasicExplicit_HH
#include "../dm/Abstract.hh"

namespace sa
{
namespace basicexplicit
{


class Solver : public sa::Solver
{
public:
    Solver(dm:Model *model);
    virtual ~Solver();
    /*
    virtual void solveIteration();

    virtual void solveArea();
    virtual void solveBound();
    virtual void solveCorner();
    */
};

class PointData : public sa::PointData
{

};


}
}

#endif
