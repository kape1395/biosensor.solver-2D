#include "BasicExplicitSA.hh"
#include <iostream>




/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Konstruktorius.
 */
sa::basicexplicit::Solver::Solver(
    cfg::Config* config,
    dm::ModelFactory* modelFactory
) : sa::Solver(config)
{
    activeLayer = 0;

    /////////////////////////////////
    //  Sukuriam duomenu modeli.
    {
        PointFactory* pointFactory = new PointFactory(config->getSubstances().size(), &activeLayer);
        this->data = new dm::Model(config, pointFactory, modelFactory);
        delete pointFactory;
    }

    /////////////////////////////////
    //  Duomenu modelio komponentams priskiriame sprendejus.
    {
        for (int i = 0; i < data->getPartsH(); i++)
            for (int j = 0; j < data->getPartsV(); j++)
            {
                dm::Area* dm = data->getArea()[i][j];
                dm->setSolver(new AreaSolver(this, dm));
            }

        for (int i = 0; i < data->getPartsH() + 1; i++)
            for (int j = 0; j < data->getPartsV(); j++)
            {
                dm::Bound* dm = data->getBoundV()[i][j];
                dm->setSolver(new BoundSolver(this, dm));
            }

        for (int i = 0; i < data->getPartsH(); i++)
            for (int j = 0; j < data->getPartsV() + 1; j++)
            {
                dm::Bound* dm = data->getBoundH()[i][j];
                dm->setSolver(new BoundSolver(this, dm));
            }

        for (int i = 0; i < data->getPartsH() + 1; i++)
            for (int j = 0; j < data->getPartsV() + 1; j++)
            {
                dm::Corner* dm = data->getCorner()[i][j];
                dm->setSolver(new CornerSolver(this, dm));
            }
    }
}




/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Destruktorius.
 */
sa::basicexplicit::Solver::~Solver()
{
    for (int i = 0; i < data->getPartsH() + 1; i++)
        for (int j = 0; j < data->getPartsV() + 1; j++)
            delete static_cast<CornerSolver*>(data->getCorner()[i][j]->getSolver());

    for (int i = 0; i < data->getPartsH() + 1; i++)
        for (int j = 0; j < data->getPartsV(); j++)
            delete static_cast<BoundSolver*>(data->getBoundV()[i][j]->getSolver());

    for (int i = 0; i < data->getPartsH(); i++)
        for (int j = 0; j < data->getPartsV() + 1; j++)
            delete static_cast<BoundSolver*>(data->getBoundH()[i][j]->getSolver());

    for (int i = 0; i < data->getPartsH(); i++)
        for (int j = 0; j < data->getPartsV(); j++)
            delete static_cast<AreaSolver*>(data->getArea()[i][j]->getSolver());

    if (data != 0)
    {
        delete data;
        data = 0;
    }
}




/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Sprendziame visa modeli.
 */
void sa::basicexplicit::Solver::solve()
{
    std::cout << "sa::basicexplicit::Solver::solve()...\n";

    for (int i = 0; i < 1000; i++)
    {
        solveIteration();
        invokeListeners();
    }

    std::cout << "sa::basicexplicit::Solver::solve()... Done\n";
}




/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Sprendziame viena iteracija pagal laika.
 */
void sa::basicexplicit::Solver::solveIteration()
{
    //std::cout << "sa::basicexplicit::Solver::solveIteration()...\n";

    activeLayer = (activeLayer + 1) % 2;

    // Sriciu vidus.
    for (int i = 0; i < data->getPartsH(); i++)
        for (int j = 0; j < data->getPartsV(); j++)
        {
            AreaSolver* solver = static_cast<AreaSolver*>(data->getArea()[i][j]->getSolver());
            solver->solveIteration();
        }

    //  Vertikalus krastai.
    for (int i = 0; i < data->getPartsH() + 1; i++)
        for (int j = 0; j < data->getPartsV(); j++)
        {
            BoundSolver* solver = static_cast<BoundSolver*>(data->getBoundV()[i][j]->getSolver());
            solver->solveIteration();
        }

    //  Horizontalus krastai.
    for (int i = 0; i < data->getPartsH(); i++)
        for (int j = 0; j < data->getPartsV() + 1; j++)
        {
            BoundSolver* solver = static_cast<BoundSolver*>(data->getBoundH()[i][j]->getSolver());
            solver->solveIteration();
        }

    //  Kampai.
    for (int i = 0; i < data->getPartsH() + 1; i++)
        for (int j = 0; j < data->getPartsV() + 1; j++)
        {
            CornerSolver* solver = static_cast<CornerSolver*>(data->getCorner()[i][j]->getSolver());
            solver->solveIteration();
        }


    //std::cout << "sa::basicexplicit::Solver::solveIteration()... Done\n";
}




/* ************************************************************************** */
/* ************************************************************************** */
/* **********   AreaSolver   ************************************************ */
/* ************************************************************************** */
/* ************************************************************************** */


/**
 *  Konstruktorius.
 */
sa::basicexplicit::AreaSolver::AreaSolver(Solver* solver, dm::Area *data)
{
    typedef std::list<cfg::Medium::Diffusion*>::iterator    CfgDiff;

    this->solver = solver;
    this->data = data;

    //////////////////////////////////////
    //  Susikonfiguruojame difuzijas.
    {
        diffusionCount = data->getConfiguration()->getMedium()->getDiffusions().size();
#ifdef AreaSolver_DIFFUSION_V1
        diffusion = new Diffusion*[diffusionCount];
#endif
#ifdef AreaSolver_DIFFUSION_V2
        diffusionSI = new int[diffusionCount];
        diffusionD  = new double[diffusionCount];
#endif

        std::list<cfg::Medium::Diffusion*>::iterator itDiff =
            data->getConfiguration()->getMedium()->getDiffusions().begin();
        for (int i = 0; i < diffusionCount; i++, itDiff++)
        {
#ifdef AreaSolver_DIFFUSION_V1
            diffusion[i] = new Diffusion(
                               solver->getData()->getSubstanceIndex((*itDiff)->getSubstance()),
                               (*itDiff)->getCoefficient()
                           );
#endif
#ifdef AreaSolver_DIFFUSION_V2
            diffusionSI[i] = solver->getData()->getSubstanceIndex((*itDiff)->getSubstance());
            diffusionD [i] = (*itDiff)->getCoefficient();
#endif
        }
    }

    //////////////////////////////////////
    //  Susirenkame reakcijas.
    {
        reactionCount = data->getConfiguration()->getMedium()->getReactions().size();
        reaction = new Reaction*[reactionCount];

        std::list<cfg::Reaction*>::iterator itReac =
            data->getConfiguration()->getMedium()->getReactions().begin();
        for (int i = 0; i < reactionCount; i++, itReac++)
        {
            if ((*itReac)->getType() == "MICHAELIS-MENTEN")
            {
                cfg::MichaelisMentenReaction* mmr = dynamic_cast<cfg::MichaelisMentenReaction*>(*itReac);
                reaction[i] = new Reaction(
                                  solver->getData()->getSubstanceIndex(mmr->getSubstrate()),
                                  solver->getData()->getSubstanceIndex(mmr->getProduct()),
                                  mmr->getV_max(),
                                  mmr->getK_M()
                              );
            }
            else
            {
                std::cerr << "ERROR: Unsupported reaction type=" << (*itReac)->getType() << '\n';
            }
        }
    }
}


/* ************************************************************************** */


/**
 *  Destruktorius.
 */
sa::basicexplicit::AreaSolver::~AreaSolver()
{
#ifdef AreaSolver_DIFFUSION_V1
    for (int i = 0; i < diffusionCount; i++)
        delete diffusion[i];
    delete[] diffusion;
#endif
#ifdef AreaSolver_DIFFUSION_V2
    delete[] diffusionSI;
    delete[] diffusionD;
#endif

    for (int i = 0; i < reactionCount; i++)
        delete reaction[i];
    delete[] reaction;
}


/* ************************************************************************** */


/**
 *  Sprendziame viena iteracija.
 */
void sa::basicexplicit::AreaSolver::solveIteration()
{
    double dt = solver->getTimeStep();
    int x = 0;
    for (data->moveToRowStart(); data->moveRight() > 0; x++)
    {
        double dx = data->getDimensionX()->getIntervals()[x];
        int y = 0;
        for (data->moveToColStart(); data->moveBottom() > 0; y++)
        {
            double dy = data->getDimensionY()->getIntervals()[y];
            // Su dynamic_cast veikia kurkas leciau.
            double* p  = static_cast<Point*>(data->getCurrent())->getLastLayerSubstances();
            double* pt = static_cast<Point*>(data->getTop    ())->getLastLayerSubstances();
            double* pb = static_cast<Point*>(data->getBottom ())->getLastLayerSubstances();
            double* pl = static_cast<Point*>(data->getLeft   ())->getLastLayerSubstances();
            double* pr = static_cast<Point*>(data->getRight  ())->getLastLayerSubstances();
            double* tl = static_cast<Point*>(data->getCurrent())->getThisLayerSubstances();

            for (int i = 0; i < diffusionCount; i++)
            {
#ifdef AreaSolver_DIFFUSION_V1
                diffusion[i]->apply(p, pt, pb, pl, pr, tl, dt, dx, dy);
#endif
#ifdef AreaSolver_DIFFUSION_V2
                double C_x  = (dt * diffusionD[i]) / (dx * dx);
                double C_y  = (dt * diffusionD[i]) / (dy * dy);
                double C_xy = 1E0 - 2E0 * C_x - 2E0 * C_y;
                int j = diffusionSI[i];
                tl[j] = C_xy * p[j] + C_x * (pr[j] + pl[j]) + C_y * (pb[j] + pt[j]);
#endif
            }

            for (int i = 0; i < reactionCount; i++)
            {
                reaction[i]->apply(p, tl, dt);
            }

        }
    }
}



/* ************************************************************************** */



sa::basicexplicit::AreaSolver::Diffusion::Diffusion(int sIndex, double coef)
{
    this->substanceIndex = sIndex;
    this->coefficient    = coef;
}
inline void sa::basicexplicit::AreaSolver::Diffusion::apply(
    double*& s, double*& st, double*& sb, double*& sl, double*& sr,
    double*& target, double& dt, double& dx, double& dy
)
{
    double C_x  = (dt * coefficient) / (dx * dx);
    double C_y  = (dt * coefficient) / (dy * dy);
    double C_xy = 1E0 - 2E0 * C_x - 2E0 * C_y;
    int i = substanceIndex;
    target[i] = C_xy * s[i] + C_x * (sr[i] + sl[i]) + C_y * (sb[i] + st[i]);
}


sa::basicexplicit::AreaSolver::Reaction::Reaction(int sIndex, int pIndex, double V_max, double K_M)
{
    this->substrateIndex = sIndex;
    this->productIndex   = pIndex;
    this->V_max          = V_max;
    this->K_M            = K_M;
}
inline void sa::basicexplicit::AreaSolver::Reaction::apply(double*& source, double*& target, double& dt)
{
    double reaction = dt * V_max * source[substrateIndex] / (K_M + source[substrateIndex]);
    target[substrateIndex] -= reaction;
    target[productIndex  ] += reaction;
}



/* ************************************************************************** */
/* ************************************************************************** */
/* **********   BoundSolver   *********************************************** */
/* ************************************************************************** */
/* ************************************************************************** */


/**
 *  BoundSolver konstruktorius.
 *  \param solver   Bendras modelio sprendejas.
 *  \param data     Krastines salygos duomenys ir konfiguracija.
 */
sa::basicexplicit::BoundSolver::BoundSolver(Solver* solver, dm::Bound* data)
{
    this->solver = solver;
    this->data   = data;

    conditionCount = data->getConfiguration()->getConditions().size();
    condition = new Condition*[conditionCount];

    std::list<cfg::Bound::Condition*>::iterator itCond =
        data->getConfiguration()->getConditions().begin();
    for (int i = 0; i < conditionCount; i++, itCond++)
    {
        if ((*itCond)->getType() == "CONSTANT")
        {
            condition[i] = new ConstCondition(
                               solver->getData()->getSubstanceIndex((*itCond)->getSubstance()),
                               dynamic_cast<cfg::Bound::ConstantCondition*>(*itCond)->getConcentration()
                           );
        }
        else if ((*itCond)->getType() == "ELECTRODE")
        {
            condition[i] = new ConstCondition(
                               solver->getData()->getSubstanceIndex((*itCond)->getSubstance()),
                               0.0
                           );
        }
        else if ((*itCond)->getType() == "WALL")
        {
            condition[i] = new WallCondition(
                               solver->getData()->getSubstanceIndex((*itCond)->getSubstance()),
                               *itCond,
                               data
                           );
        }
        else if ((*itCond)->getType() == "MERGE")
        {
            condition[i] = new MergeCondition(
                               solver->getData()->getSubstanceIndex((*itCond)->getSubstance()),
                               dynamic_cast<cfg::Bound::MergeCondition*>(*itCond),
                               data
                           );
        }
        else
        {
            std::cerr << "ERROR: Unknown bound conditio type=" << (*itCond)->getType() << '\n';
        }
    }
}


/* ************************************************************************** */


/**
 *  BoundSolver destruktorius.
 */
sa::basicexplicit::BoundSolver::~BoundSolver()
{
    for (int i = 0; i < conditionCount; i++)
        delete condition[i];
    delete[] condition;
}


/* ************************************************************************** */


/**
 *  Sprenziame viena iteracija vienam krastui.
 */
void sa::basicexplicit::BoundSolver::solveIteration()
{
    for (data->moveToStart(); data->moveNext() > 0; )
    {
        for (int i = 0; i < conditionCount; i++)
        {
            condition[i]->apply(data);
        }
    }
}


/* ************************************************************************** */


sa::basicexplicit::BoundSolver::Condition::Condition(int substanceIndex)
{
    this->substanceIndex = substanceIndex;
}
sa::basicexplicit::BoundSolver::Condition::~Condition()
{}


sa::basicexplicit::BoundSolver::WallCondition::WallCondition(
    int                    substanceIndex,
    cfg::Bound::Condition* cgf,
    dm::Bound*             data
) : Condition(substanceIndex)
{

    bool foundInPrev = false;
    bool foundInNext = false;
    std::list<cfg::Medium::Diffusion*>::iterator itDiff;
    if (data->getPrevArea() != 0)
    {
        for (itDiff = data->getPrevArea()->getConfiguration()->getMedium()->getDiffusions().begin();
                itDiff != data->getPrevArea()->getConfiguration()->getMedium()->getDiffusions().end();
                itDiff++)
        {
            if ((cgf->getSubstance() == (*itDiff)->getSubstance()) && (*itDiff)->getCoefficient() != 0.0)
            {
                foundInPrev = true;
                break;
            }
        }
    }
    if (data->getNextArea() != 0)
    {
        for (itDiff = data->getNextArea()->getConfiguration()->getMedium()->getDiffusions().begin();
                itDiff != data->getNextArea()->getConfiguration()->getMedium()->getDiffusions().end();
                itDiff++)
        {
            if ((cgf->getSubstance() == (*itDiff)->getSubstance()) && (*itDiff)->getCoefficient() != 0.0)
            {
                foundInNext = true;
                break;
            }
        }
    }

    if (foundInPrev && foundInNext)
        std::cout << "ERROR: wall condition with both sides.\n";
    if (!foundInPrev && !foundInNext)
        std::cout << "ERROR: wall condition without sides.\n";

    targetArea = foundInPrev ? data->getPrevArea() : data->getNextArea();
}
sa::basicexplicit::BoundSolver::WallCondition::~WallCondition()
{}
void sa::basicexplicit::BoundSolver::WallCondition::apply(dm::Bound* data)
{
    Point* dst = dynamic_cast<Point*>(data->getCurrent());
    Point* src = dynamic_cast<Point*>((targetArea == data->getPrevArea())
                                      ? data->getPrevAreaPoint()
                                      : data->getNextAreaPoint()
                                     );

    dst->getThisLayerSubstances()[substanceIndex] = src->getThisLayerSubstances()[substanceIndex];
}



/**
 *
 */
sa::basicexplicit::BoundSolver::MergeCondition::MergeCondition(
    int                         substanceIndex,
    cfg::Bound::MergeCondition* cfg,
    dm::Bound*                  data
) : Condition(substanceIndex)
{
    typedef std::list<cfg::Medium::Diffusion*> CfgDList;
    nextArea_D = 0.0;
    {
        CfgDList diffs = data->getNextArea()->getConfiguration()->getMedium()->getDiffusions();
        for (CfgDList::iterator it = diffs.begin(); it != diffs.end(); it++)
            if ((*it)->getSubstance() == cfg->getSubstance())
            {
                nextArea_D = (*it)->getCoefficient();
                break;
            }
    }

    prevArea_D = 0.0;
    {
        CfgDList diffs = data->getPrevArea()->getConfiguration()->getMedium()->getDiffusions();
        for (CfgDList::iterator it = diffs.begin(); it != diffs.end(); it++)
            if ((*it)->getSubstance() == cfg->getSubstance())
            {
                prevArea_D = (*it)->getCoefficient();
                break;
            }
    }

    if (data->getDimension()->getDirection() == dm::HORIZONTAL)
    {
        int e = data->getPrevArea()->getDimensionY()->getPointCount() - 2;
        nextArea_dx = data->getNextArea()->getDimensionY()->getIntervals()[0];
        prevArea_dx = data->getPrevArea()->getDimensionY()->getIntervals()[e];
    }
    else
    {
        int e = data->getPrevArea()->getDimensionX()->getPointCount() - 2;
        nextArea_dx = data->getNextArea()->getDimensionX()->getIntervals()[0];
        prevArea_dx = data->getPrevArea()->getDimensionX()->getIntervals()[e];
    }

}
/**
 *
 */
sa::basicexplicit::BoundSolver::MergeCondition::~MergeCondition()
{}
/**
 *
 */
void sa::basicexplicit::BoundSolver::MergeCondition::apply(dm::Bound* data)
{
    double a = prevArea_D / prevArea_dx;
    double c = nextArea_D / nextArea_dx;
    double b = -(a + c);
    double* pt = dynamic_cast<Point*>(data->getCurrent())->getThisLayerSubstances();
    double* pp = dynamic_cast<Point*>(data->getPrevAreaPoint())->getThisLayerSubstances();
    double* pn = dynamic_cast<Point*>(data->getNextAreaPoint())->getThisLayerSubstances();

    pt[substanceIndex] = -(a * pp[substanceIndex] + c * pn[substanceIndex]) / b;
}


/**
 *
 */
sa::basicexplicit::BoundSolver::ConstCondition::ConstCondition(
    int substanceIndex, double concentration
) : Condition(substanceIndex)
{
    this->concentration = concentration;
}
/**
 *
 */
sa::basicexplicit::BoundSolver::ConstCondition::~ConstCondition()
{}
/**
 *
 */
void sa::basicexplicit::BoundSolver::ConstCondition::apply(dm::Bound* data)
{
    Point* point = dynamic_cast<Point*>(data->getCurrent());
    point->getThisLayerSubstances()[substanceIndex] = concentration;
}





/* ************************************************************************** */
/* ************************************************************************** */
/* **********   CornerSolver   ********************************************** */
/* ************************************************************************** */
/* ************************************************************************** */


/**
 *  Sprendcia sriciu kampa.
 *  FIXME: Atrodo cia kazkas negerai skaiciuojasi, reikia realizuoti normaliai.
 *         Tipo prirasiau sudo, ir dar noriu kad veiktu.
 */
void sa::basicexplicit::CornerSolver::solveIteration()
{
    std::list<cfg::Substance*>::iterator sBeg = solver->getData()->getConfiguration()->getSubstances().begin();
    std::list<cfg::Substance*>::iterator sEnd = solver->getData()->getConfiguration()->getSubstances().end();
    std::list<cfg::Substance*>::iterator sCur;
    for (sCur = sBeg; sCur != sEnd; sCur++)
    {
        cfg::Bound::Condition* rCond = 0;
        cfg::Bound::Condition* lCond = 0;
        cfg::Bound::Condition* tCond = 0;
        cfg::Bound::Condition* bCond = 0;

        std::list<cfg::Bound::Condition*>::iterator bcBeg;
        std::list<cfg::Bound::Condition*>::iterator bcEnd;
        std::list<cfg::Bound::Condition*>::iterator bcCur;
        if (data->getRightBound() != 0)
        {
            bcBeg = data->getRightBound()->getConfiguration()->getConditions().begin();
            bcEnd = data->getRightBound()->getConfiguration()->getConditions().end();
            for (bcCur = bcBeg; bcCur != bcEnd; bcCur++)
            {
                if ((*bcCur)->getSubstance() == *sCur)
                    rCond = *bcCur;
            }
        }
        if (data->getLeftBound() != 0)
        {
            bcBeg = data->getLeftBound()->getConfiguration()->getConditions().begin();
            bcEnd = data->getLeftBound()->getConfiguration()->getConditions().end();
            for (bcCur = bcBeg; bcCur != bcEnd; bcCur++)
            {
                if ((*bcCur)->getSubstance() == *sCur)
                    lCond = *bcCur;
            }
        }
        if (data->getTopBound() != 0)
        {
            bcBeg = data->getTopBound()->getConfiguration()->getConditions().begin();
            bcEnd = data->getTopBound()->getConfiguration()->getConditions().end();
            for (bcCur = bcBeg; bcCur != bcEnd; bcCur++)
            {
                if ((*bcCur)->getSubstance() == *sCur)
                    tCond = *bcCur;
            }
        }
        if (data->getBottomBound() != 0)
        {
            bcBeg = data->getBottomBound()->getConfiguration()->getConditions().begin();
            bcEnd = data->getBottomBound()->getConfiguration()->getConditions().end();
            for (bcCur = bcBeg; bcCur != bcEnd; bcCur++)
            {
                if ((*bcCur)->getSubstance() == *sCur)
                    bCond = *bcCur;
            }
        }

        Point* cp = dynamic_cast<Point*>(data->getCurrent());           // Current point
        Point* tp = 0;                                                  // Temporary point
        int    si = this->solver->getData()->getSubstanceIndex(*sCur);  // Current substance index
        double sum   = 0.0;
        int    count = 0;

        if (rCond != 0)
        {
            data->getRightBound()->moveToStart();
            tp = dynamic_cast<Point*>(data->getRightBound()->getCurrent());
            sum += tp->getThisLayerSubstances()[si];
            count++;
        }
        if (lCond != 0)
        {
            data->getLeftBound()->moveToEnd();
            tp = dynamic_cast<Point*>(data->getLeftBound()->getCurrent());
            sum += tp->getThisLayerSubstances()[si];
            count++;
        }
        if (tCond != 0)
        {
            data->getTopBound()->moveToEnd();
            tp = dynamic_cast<Point*>(data->getTopBound()->getCurrent());
            sum += tp->getThisLayerSubstances()[si];
            count++;
        }
        if (bCond != 0)
        {
            data->getBottomBound()->moveToStart();
            tp = dynamic_cast<Point*>(data->getBottomBound()->getCurrent());
            sum += tp->getThisLayerSubstances()[si];
            count++;
        }

        cp->getThisLayerSubstances()[si] = sum / count;
    }
}


/* ************************************************************************** */
