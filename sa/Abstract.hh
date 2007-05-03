/// \file
/// \todo Nezinau dar tiksliai, kaip cia ka padaryti...
///
#ifndef SA_Abstract_HH
#define SA_Abstract_HH
#include "../dm/Abstract.hh"

namespace sa
{


/**
 *
 */
class Solver
{
public:
    Solver();
    virtual ~Solver();

    virtual void solveIteration() = 0;

};


/**
 *
 */
class AreaSolver
{
protected:
    dm::Area<double> *data;

public:
    AreaSolver(dm::Area<double> *data);
    virtual ~AreaSolver();

};



/**
 *
 */
class BoundSolver
{
public:
    BoundSolver();
    virtual ~BoundSolver();

    enum Type {
        CONST, MERGE, WALL
    };

};



/**
 *
 */
class CornerSolver
{
public:
    CornerSolver();
    virtual ~CornerSolver();
};


class PointData
{

};


}   // namespace sa
#endif
