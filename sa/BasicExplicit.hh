#ifndef SA_BasicExplicit_HH
#define SA_BasicExplicit_HH
#include "Abstract.hh"
#include "../dm/Abstract.hh"
#include <map>

namespace sa
{
namespace basicexplicit
{

// TODO: Cia padaryti isreikstine schema.
// TODO: I Model strukturas ideti biojutiklio parametrus.


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *
 */
class Solver : public sa::Solver
{
protected:
    //void*** solvers;        // 2D Array of pointers.    [H*2+1][V*2+1]
    std::map<void*, void*> solvers;

    virtual void solveIteration();

public:
    Solver(dm::Model* data);
    virtual ~Solver();
    virtual void solve();

};




/* ************************************************************************** */
/* ************************************************************************** */
/**
 *
 */
class AreaSolver
{
protected:
    dm::Area* data;

public:
    AreaSolver(dm::Area* data)
    {
        this->data = data;
    }
    virtual ~AreaSolver()
    {}
    virtual void solveIteration();

};




/* ************************************************************************** */
/* ************************************************************************** */
/**
 *
 */
class BoundSolver
{
protected:
    dm::Bound* data;

public:
    BoundSolver(dm::Bound* data)
    {
        this->data = data;
    }
    virtual ~BoundSolver()
    {}
    virtual void solveIteration();

protected:
    class Wall
        {};
    class Merge
        {};
    class Const
        {};
};




/* ************************************************************************** */
/* ************************************************************************** */
/**
 *
 */
class CornerSolver
{
protected:
    dm::Corner* data;

public:
    CornerSolver(dm::Corner* data)
    {
        this->data = data;
    }
    virtual ~CornerSolver()
    {}
    virtual void solveIteration();

};




/* ************************************************************************** */
/* ************************************************************************** */
}   // namespace basicexplicit
}   // namespace sa

#endif
