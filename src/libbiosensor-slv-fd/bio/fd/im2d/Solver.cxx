#include "Solver.hxx"
#include <bio/Exception.hxx>


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::Solver::Solver(BIO_XML_NS::model::Model* config) :
AbstractIterativeSolver(config),
        log(log4cxx::Logger::getLogger("libbiosensor-slv-fd::im2d::Solver"))
{
    constructSolver();
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::Solver::~Solver()
{
    for (int h = 0; h < subSolvers->sizeH(); h++)
    {
        for (int v = 0; v < subSolvers->sizeV(); v++)
        {
            delete subSolvers->getAreas()[h][v];
        }
    }
    // FIXME: Implement, bounds/corners...
    delete subSolvers;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::Solver::solveIteration()
{ 
    LOG4CXX_DEBUG(log, "solveIteration()");
    for (int h = 0; h < subSolvers->sizeH(); h++)
    {
        for (int v = 0; v < subSolvers->sizeV(); v++)
        {
            subSolvers->getAreas()[h][v]->solveFirstHalfStep();
        }
    }
    for (int v = 0; v < subSolvers->sizeV(); v++)
    {
        for (int h = 0; h < subSolvers->sizeH(); h++)
        {
            subSolvers->getAreas()[h][v]->solveSecondHalfStep();
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::Solver::constructSolver()
{
    LOG4CXX_DEBUG(log, "constructSolver()...");
    structAnalyzer.analyze(getConfig());
    
    if (!structAnalyzer.isTwoDimensional())
    {
        LOG4CXX_ERROR(log, "Config is not two-dimensional, this solver supports only two-dimensional models");
        throw Exception("Unsuppordet model");
    }
    
    char message[1000];
    
    subSolvers = new SplittedSolver(
            structAnalyzer.getPointsH().size() - 1,
            structAnalyzer.getPointsV().size() - 1
            );
    
    sprintf(message, "SubSolver grid has sizeH=%i sizeV=%i", subSolvers->sizeH(), subSolvers->sizeV());
    LOG4CXX_DEBUG(log, message);
    
    
    ////////////////////////////////////////////////////////////////////////////
    //  Initialize area solvers.
    //
    for (int h = 0; h < subSolvers->sizeH(); h++)
    {
        for (int v = 0; v < subSolvers->sizeV(); v++)
        {
            subSolvers->getAreas()[h][v] = new AreaSubSolver(h, v, &structAnalyzer);
        }
    }
    //
    //  Initialize area solvers.
    ////////////////////////////////////////////////////////////////////////////
    
    
    LOG4CXX_DEBUG(log, "constructSolver()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
