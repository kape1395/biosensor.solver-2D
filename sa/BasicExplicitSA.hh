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
 *  Paprastas sprendejas, naujojantis isreikstine schema.
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

    virtual double getTimeStep()    // FIXME: Kazkaip normaliai realizuoti.
    {
        return 1E-6;
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

#define AreaSolver_DIFFUSION_V1
#undef  AreaSolver_DIFFUSION_V2

/**
 *  Sprendcia srities vidu.
 */
class AreaSolver
{
protected:
    class Diffusion;
    class Reaction;
    Solver*     solver;                 ///< Pagrindinis sprendejas.
    dm::Area*   data;                   ///< Duomenys su kuriais dirbame.

    int         diffusionCount;         ///< Kiek turime difuziju.
#ifdef AreaSolver_DIFFUSION_V1
    Diffusion** diffusion;              ///< Difuziju objektai.
#endif
#ifdef AreaSolver_DIFFUSION_V2
    int*        diffusionSI;            ///< Substance Index
    double*     diffusionD;             ///< Difuzijos koef.
#endif

    Reaction**  reaction;           ///< Reakciju objektai.
    int         reactionCount;      ///< Kiek turime reakciju.

public:
    AreaSolver(Solver* solver, dm::Area* data);
    virtual ~AreaSolver();
    virtual void solveIteration();

protected:
#ifdef AreaSolver_DIFFUSION_V1
    /// Modeliuoja difuzija.
    class Diffusion
    {
    protected:
        int substanceIndex;
        double coefficient;
    public:
        Diffusion(int sIndex, double coef);
        inline void apply(
            double*& s, double*& st, double*& sb, double*& sl, double*& sr,
            double*& target, double& dt, double& dx, double& dy
        );
    };

    /// Modeliuoja Michaelis-Menten reakcija.
    class Reaction
    {
    protected:
        int substrateIndex;
        int productIndex;
        double V_max;
        double K_M;
    public:
        Reaction(int sIndex, int pIndex, double V_max, double K_M);
        inline void apply(double*& source, double*& target, double& dt);
    };
#endif
};





/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Sprendzia srities krastus.
 */
class BoundSolver
{
protected:
    class Condition;
    class WallCondition;
    class MergeCondition;
    class ConstCondition;
    Solver*     solver;                 ///< Pagrindinis sprendejas.
    dm::Bound*  data;                   ///< Duomenys su kuriais dirbame.

    int         conditionCount;         ///< Kiek turime krastiniu salygu.
    Condition** condition;              ///< Krastu sprendejai konkreciom medziagom.

public:
    BoundSolver(Solver* solver, dm::Bound* data);
    virtual ~BoundSolver();
    virtual void solveIteration();

};

/// Abstrakti krastine salyga.
class BoundSolver::Condition
{
protected:
    int substanceIndex;
public:
    Condition(int substanceIndex);
    virtual ~Condition();
    virtual void apply(dm::Bound* data) = 0;
};

/// Nepratekejimo salyga.
class BoundSolver::WallCondition : public Condition
{
protected:
    dm::Area* targetArea;
public:
    WallCondition(int substanceIndex, cfg::Bound::Condition* cgf, dm::Bound* data);
    virtual ~WallCondition();
    virtual void apply(dm::Bound* data);
};

/// Derinimo salyga.
class BoundSolver::MergeCondition : public Condition
{
protected:
    double nextArea_D;
    double nextArea_dx;
    double prevArea_D;
    double prevArea_dx;
public:
    MergeCondition(int substanceIndex, cfg::Bound::MergeCondition* cfg, dm::Bound* data);
    virtual ~MergeCondition();
    virtual void apply(dm::Bound* data);
};

/// Pastovios koncentracijos salyga.
class BoundSolver::ConstCondition : public Condition
{
protected:
    double concentration;
public:
    ConstCondition(int substanceIndex, double concentration);
    virtual ~ConstCondition();
    virtual void apply(dm::Bound* data);
};



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Sprendzia sriciu kampus.
 */
class CornerSolver
{
protected:
    Solver*     solver;
    dm::Corner* data;

public:
    CornerSolver(Solver* solver, dm::Corner* data)
    {
        this->solver = solver;
        this->data   = data;
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
