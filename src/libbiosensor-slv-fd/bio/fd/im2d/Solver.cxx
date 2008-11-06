#include "Solver.hxx"
#include <bio/Exception.hxx>


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::Solver::Solver(BIO_XML_NS::model::Model* config) :
        AbstractIterativeSolver(config),
        log(log4cxx::Logger::getLogger("libbiosensor-slv-fd::im2d::Solver"))
{
    LOG4CXX_DEBUG(log, "Solver()...");
    structAnalyzer.analyze(getConfig());
    fdAnalyzer.analyze(getConfig());

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
    for (int h = 0; h <= subSolvers->sizeH(); h++)
    {
        for (int v = 0; v <= subSolvers->sizeV(); v++)
        {
            bool lastH = h == subSolvers->sizeH();
            bool lastV = v == subSolvers->sizeV();

            if (!(lastH || lastV))
                subSolvers->getAreas()[h][v] = new AreaSubSolver(h, v, &structAnalyzer, &fdAnalyzer);

            if (!lastH)
                subSolvers->getBoundsH()[h][v] = new BoundSubSolver(h, v, true, &structAnalyzer, &fdAnalyzer);

            if (!lastV)
                subSolvers->getBoundsV()[h][v] = new BoundSubSolver(h, v, false, &structAnalyzer, &fdAnalyzer);

            subSolvers->getCorners()[h][v] = new CornerSubSolver(h, v, &structAnalyzer, &fdAnalyzer);
        }
    }
    //
    //  Initialize area solvers.
    ////////////////////////////////////////////////////////////////////////////


    LOG4CXX_DEBUG(log, "Solver()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::Solver::~Solver()
{
    for (int h = 0; h <= subSolvers->sizeH(); h++)
    {
        for (int v = 0; v <= subSolvers->sizeV(); v++)
        {
            bool lastH = h == subSolvers->sizeH();
            bool lastV = v == subSolvers->sizeV();

            if (!(lastH || lastV)) delete subSolvers->getAreas()[h][v];
            if (!lastH) delete subSolvers->getBoundsH()[h][v];
            if (!lastV) delete subSolvers->getBoundsV()[h][v];
            delete subSolvers->getCorners()[h][v];
        }
    }
    delete subSolvers;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::Solver::solveIteration()
{
    LOG4CXX_DEBUG(log, "solveIteration()");

    ////////////////////////////////////////////////////////////////////////////
    //  Solve "horizintal" half-step in time
    //
    for (int h = 0; h < subSolvers->sizeH(); h++)
    {
        for (int v = 0; v < subSolvers->sizeV(); v++)
        {
            bool lastH = h == subSolvers->sizeH();
            bool lastV = v == subSolvers->sizeV();

            if (!lastV)
                subSolvers->getBoundsV()[h][v]->solveThrough();

            if (!(lastH || lastV))
                subSolvers->getAreas()[h][v]->solveHorizontal();

            if (!lastH)
                subSolvers->getBoundsH()[h][v]->solveAlong();

            subSolvers->getCorners()[h][v]->solve();
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    //  Solve "vertical" half-step in time
    //
    for (int h = 0; h < subSolvers->sizeH(); h++)
    {
        for (int v = 0; v < subSolvers->sizeV(); v++)
        {
            bool lastH = h == subSolvers->sizeH();
            bool lastV = v == subSolvers->sizeV();

            if (!lastH)
                subSolvers->getBoundsH()[h][v]->solveThrough();

            if (!(lastH || lastV))
                subSolvers->getAreas()[h][v]->solveVertical();

            if (!lastV)
                subSolvers->getBoundsV()[h][v]->solveAlong();

            subSolvers->getCorners()[h][v]->solve();
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    //  TODO: Switch current and previous data-layers
    //

}


/* ************************************************************************** */
/* ************************************************************************** */
