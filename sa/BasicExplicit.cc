#include "BasicExplicit.hh"
#include <iostream>




/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Konstruktorius.
 */
sa::basicexplicit::Solver::Solver(dm::Model* data) : sa::Solver(data)
{
    /*
    solvers = new void**[data->getPartsH() * 2 + 1];
    for (int i = 0; i < data->getPartsH() * 2 + 1; i++)
        solvers[i] = new void*[data->getPartsV() * 2 + 1];


    for (int i = 0; i < data->getPartsH(); i++)
        for (int j = 0; j < data->getPartsV(); j++)
        {
            dm::Area area = data->getAreas()[i][j];

        }
    */
    // TODO: Sudelioti solver`ius pagal konfiguracija.

    for (int i = 0; i < data->getPartsH(); i++)
        for (int j = 0; j < data->getPartsV(); j++)
        {
            dm::Area* dm = data->getArea()[i][j];
            AreaSolver* solver = new AreaSolver(dm);
            solvers[dm] = solver;
        }



}




/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Destruktorius.
 */
sa::basicexplicit::Solver::~Solver()
{
    /*
    for (int i = 0; i < data->getPartsH() * 2 + 1; i++)
        delete[] solvers[i];
    delete[] solvers;
    */
//  for (; !solvers.empty(); solvers.pop_front())
//      delete (*solvers.begin()).second;
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
    /*

    // Sriciu vidus.
    for (int i = 0; i < data->getPartsH(); i++)
        for (int j = 0; j < data->getPartsV(); j++)
        {
            AreaSolver* solver = static_cast<AreaSolver*>(solvers[i * 2 + 1][j * 2 + 1]);
            solver->solveIteration();
        }

    //  Vertikalus krastai.
    for (int i = 0; i < data->getPartsH() + 1; i++)
        for (int j = 0; j < data->getPartsV(); j++)
        {
            BoundSolver* solver = static_cast<BoundSolver*>(solvers[i * 2][j * 2 + 1]);
            solver->solveIteration();
        }

    //  Horizontalus krastai.
    for (int i = 0; i < data->getPartsH(); i++)
        for (int j = 0; j < data->getPartsV() + 1; j++)
        {
            BoundSolver* solver = static_cast<BoundSolver*>(solvers[i * 2 + 1][j * 2]);
            solver->solveIteration();
        }

    //  Kampai.
    for (int i = 0; i < data->getPartsH() + 1; i++)
        for (int j = 0; j < data->getPartsV() + 1; j++)
        {
            CornerSolver* solver = static_cast<CornerSolver*>(solvers[i * 2][j * 2]);
            solver->solveIteration();
        }

    */
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



