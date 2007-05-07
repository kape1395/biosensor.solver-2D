#ifndef SA_BasicExplicitSA_HH
#define SA_BasicExplicitSA_HH
#include "AbstractSA.hh"
#include "../dm/AbstractDM.hh"

namespace sa
{
namespace basicexplicit
{



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *
 */
class Solver : public sa::Solver
{
protected:
    int activeLayer;    ///< Skaiciuojamo sluoksnio numeris (0, 1).

    virtual void solveIteration();

    /// Sita konstruktoriu gali kvieti tik sunines klases...
    Solver(cfg::Config* config, dm::Model* model) : sa::Solver(config, model)
    {}

    /// Sita konstruktoriu gali kvieti tik sunines klases...
    Solver(cfg::Config* config) : sa::Solver(config)
    {}

public:
    Solver(cfg::Config* config, dm::ModelFactory* modelFactory);
    virtual ~Solver();
    virtual void solve();

    virtual double getTimeStep()
    {
        return 1E-4;
    }

};




/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Modelio tasko duomenys.
 */
class Point : public dm::Point
{
protected:
    int*     activeLayer;   // 0/1
    double** substances;

public:
    /// Konstruktorius.
    Point(
        int substanceCount,
        int* layerPointer
    ) : dm::Point()
    {
        substances = new double*[2];
        substances[0] = new double[substanceCount];
        substances[1] = new double[substanceCount];
        activeLayer = layerPointer;
    }

    /// Destruktorius.
    virtual ~Point()
    {
        delete[] substances[0];
        delete[] substances[1];
        delete[] substances;
    }

    /// dm::Point implementation.
    virtual double getSubstance(int substanceNr)
    {
        return substances[*activeLayer][substanceNr];
    }

    /// Grazina skaiciuojamo sluoksnio koncentracijas.
    virtual double* getThisLayerSubstances()
    {
        return substances[((*activeLayer) + 1) % 2];
    }

    /// Grazina pereito suskaiciuoto sluoksnio koncentracijas.
    virtual double* getLastLayerSubstances()
    {
        return substances[*activeLayer];
    }
};






/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Tasku gamykla.
 */
class PointFactory : public dm::PointFactory
{
protected:
    int substanceCount;
    int* layerPointer;

public:
    /// Konstruktorius.
    PointFactory(
        int substanceCount,
        int* layerPointer
    ) : dm::PointFactory()
    {
        this->substanceCount = substanceCount;
        this->layerPointer   = layerPointer;
    }

    /// Destruktorius.
    virtual ~PointFactory()
    {}

    /// Interfeiso dm::PointFactory implementacija.
    virtual Point* newPoint()
    {
        return new Point(substanceCount, layerPointer);
    }

};




/* ************************************************************************** */
/* ************************************************************************** */
/**
 *
 */
class AreaSolver
{
protected:
    Solver*     solver;                 ///< Pagrindinis sprendejas.
    dm::Area*   data;                   ///< Duomenys su kuriais dirbame.
    int         substanceCount;         ///< Difunduojanciu medziagu kiekis.
    double*     diffusionCoefficient;   ///< Difuziju koeficientai.

public:
    AreaSolver(Solver* solver, dm::Area* data);
    virtual ~AreaSolver();
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
