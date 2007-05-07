#include "BasicExplicitSA.hh"
#include <iostream>




/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Konstruktorius.
 */
sa::basicexplicit::Solver::Solver(dm::Model* data) : sa::Solver(data)
{
    for (int i = 0; i < data->getPartsH(); i++)
        for (int j = 0; j < data->getPartsV(); j++)
        {
            dm::Area* dm = data->getArea()[i][j];
            dm->setSolver(new AreaSolver(dm));
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
}




/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Sprendziame visa modeli.
 */
void sa::basicexplicit::Solver::solve()
{
    std::cout << "sa::basicexplicit::Solver::solve()...\n";

    for (int i = 0; i < 2; i++)
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
    std::cout << "sa::basicexplicit::Solver::solveIteration()...\n";


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


    std::cout << "sa::basicexplicit::Solver::solveIteration()... Done\n";
}




/* ************************************************************************** */
/* ************************************************************************** */

void sa::basicexplicit::AreaSolver::solveIteration()
{
    // TODO: Implement
}


void sa::basicexplicit::BoundSolver::solveIteration()
{
    // TODO: Implement
}


void sa::basicexplicit::CornerSolver::solveIteration()
{
    // TODO: Implement
}



