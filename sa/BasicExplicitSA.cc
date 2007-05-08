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
                dm->setSolver(new BoundSolver(dm));
            }

        for (int i = 0; i < data->getPartsH(); i++)
            for (int j = 0; j < data->getPartsV() + 1; j++)
            {
                dm::Bound* dm = data->getBoundH()[i][j];
                dm->setSolver(new BoundSolver(dm));
            }

        for (int i = 0; i < data->getPartsH() + 1; i++)
            for (int j = 0; j < data->getPartsV() + 1; j++)
            {
                dm::Corner* dm = data->getCorner()[i][j];
                dm->setSolver(new CornerSolver(dm));
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



void sa::basicexplicit::BoundSolver::solveIteration()
{
    //std::cout << '[';
    for (data->moveToStart(); data->moveNext() > 0; )
    {
        // TODO: Implement
        //std::cout << '-';
    }
    //std::cout << ']';
}


void sa::basicexplicit::CornerSolver::solveIteration()
{
    // TODO: Implement
    //data->
    //std::cout << '+';
}



