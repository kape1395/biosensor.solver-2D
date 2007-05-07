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
/* **********   AreaSolver   ************************************************ */
/* ************************************************************************** */

/**
 *  Konstruktorius.
 */
sa::basicexplicit::AreaSolver::AreaSolver(Solver* solver, dm::Area *data)
{
    this->solver = solver;
    this->data = data;
    this->substanceCount = solver->getData()->getConfiguration()->getSubstances().size();

    //////////////////////////////////////
    //  Susirenkam difuzijos koeficientus.
    std::list<cfg::Substance*>::iterator substance = solver->getData()->getConfiguration()->getSubstances().begin();
    this->diffusionCoefficient = new double[substanceCount];
    for (int i = 0; i < substanceCount; i++, substance++)
    {
        diffusionCoefficient[i] = 0.0;
        std::list<cfg::Medium::Diffusion*>::iterator diff = data->getConfiguration()->getMedium()->getDiffusions().begin();
        std::list<cfg::Medium::Diffusion*>::iterator dend = data->getConfiguration()->getMedium()->getDiffusions().end();
        for ( ; diff != dend; diff++)
        {
            if ((*diff)->getSubstance() == (*substance))
            {
                diffusionCoefficient[i] = (*diff)->getCoefficient();
            }
        }
    }
    // TODO: Reactions
}

/* ************************************************************************** */

/**
 *  Destruktorius.
 */
sa::basicexplicit::AreaSolver::~AreaSolver()
{
    delete[] diffusionCoefficient;
}

/* ************************************************************************** */

/**
 *  Sprendziame viena iteracija.
 */
void sa::basicexplicit::AreaSolver::solveIteration()
{
    int x = 0;
    for (data->moveToRowStart(); data->moveRight() > 0; x++)
    {
        int y = 0;
        for (data->moveToColStart(); data->moveBottom() > 0; y++)
        {
            // Su dynamic_cast veikia kurkas leciau.
            double* p  = static_cast<Point*>(data->getCurrent())->getLastLayerSubstances();
            double* pt = static_cast<Point*>(data->getTop    ())->getLastLayerSubstances();
            double* pb = static_cast<Point*>(data->getBottom ())->getLastLayerSubstances();
            double* pl = static_cast<Point*>(data->getLeft   ())->getLastLayerSubstances();
            double* pr = static_cast<Point*>(data->getRight  ())->getLastLayerSubstances();
            double* tl = static_cast<Point*>(data->getCurrent())->getThisLayerSubstances();
            for (int i = 0; i < substanceCount; i++)
            {
                if (diffusionCoefficient[i] == 0.0)
                    continue;

                double dx = data->getDimensionX()->getIntervals()[x];
                double dy = data->getDimensionY()->getIntervals()[y];
                double dt = solver->getTimeStep();
                double D  = diffusionCoefficient[i];
                double C_x  = (dt * D) / (dx * dx);
                double C_y  = (dt * D) / (dy * dy);
                double C_xy = 1E0 - 2E0 * C_x - 2E0 * C_y;

                tl[i] = C_xy * p[i] + C_x * (pr[i] + pl[i]) + C_y * (pb[i] + pt[i]);
            }
            // TODO: Reactions
        }
    }
}


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



