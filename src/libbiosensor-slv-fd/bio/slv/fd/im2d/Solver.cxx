#include "bio/slv/AbstractIterativeSolver.hxx"
#include "Solver.hxx"
#include <bio/Exception.hxx>


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::Solver::Solver(BIO_XML_NS::model::Model* config) :
        AbstractIterativeSolver(config),
        log(log4cxx::Logger::getLogger("libbiosensor-slv-fd.im2d.Solver"))
{
    structAnalyzer = new BIO_CFG_NS::StructureAnalyzer(config);
    boundAnalyzer = new BIO_CFG_NS::BoundAnalyzer(structAnalyzer);
    fdAnalyzer = new BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer(config);

    LOG4CXX_DEBUG(log, "Solver()...");

    if (!structAnalyzer->isTwoDimensional())
    {
        LOG4CXX_ERROR(log, "Config is not two-dimensional, this solver supports only two-dimensional models");
        throw Exception("Unsuppordet model");
    }

    char message[1000];

    subSolvers = new SplittedSolver(
        structAnalyzer->getPointsH().size() - 1,
        structAnalyzer->getPointsV().size() - 1
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
                subSolvers->getAreas()[h][v] = new AreaSubSolver(
                    this, h, v,
                    structAnalyzer, fdAnalyzer
                );

            if (!lastH)
                subSolvers->getBoundsH()[h][v] = new BoundSubSolver( // Horizontal Bound
                    this, h, v, true,
                    v > 0 ? subSolvers->getAreas()[h][v - 1] : 0,
                    !lastV ? subSolvers->getAreas()[h][v] : 0,
                    structAnalyzer, fdAnalyzer, boundAnalyzer
                );

            if (!lastV)
                subSolvers->getBoundsV()[h][v] = new BoundSubSolver( // Vertical Bound
                    this, h, v, false,
                    h > 0 ? subSolvers->getAreas()[h - 1][v] : 0,
                    !lastH ? subSolvers->getAreas()[h][v] : 0,
                    structAnalyzer, fdAnalyzer, boundAnalyzer
                );

            subSolvers->getCorners()[h][v] = new CornerSubSolver(h, v, structAnalyzer, fdAnalyzer);
        }
    }
    //
    //  Initialize area solvers.
    ////////////////////////////////////////////////////////////////////////////


    this->dataModel = new DataModel(this, structAnalyzer, fdAnalyzer);
    this->setTimeStep(fdAnalyzer->getTimeStep());


    LOG4CXX_DEBUG(log, "Solver()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::Solver::~Solver()
{
    LOG4CXX_DEBUG(log, "~Solver()");
    delete dataModel;

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

    delete fdAnalyzer;
    delete boundAnalyzer;
    delete structAnalyzer;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_DM_NS::IDataModel* BIO_SLV_FD_IM2D_NS::Solver::getData()
{
    return dataModel;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::Solver::SplittedSolver* BIO_SLV_FD_IM2D_NS::Solver::getSubSolvers()
{
    return subSolvers;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::Solver::solveIteration()
{
    LOG4CXX_DEBUG(log, "solveIteration()...");

    ////////////////////////////////////////////////////////////////////////////
    //  Solve "horizintal" half-step in time
    //
    LOG4CXX_DEBUG(log, "solveIteration: Solve horizintal half-step in time, Forward");
    for (int h = 0; h <= subSolvers->sizeH(); h++)   // Forward
    {
        for (int v = 0; v <= subSolvers->sizeV(); v++)
        {
            bool lastH = h == subSolvers->sizeH();
            bool lastV = v == subSolvers->sizeV();

            if (!lastV)
                subSolvers->getBoundsV()[h][v]->solveThroughForward();

            if (!(lastH || lastV))
                subSolvers->getAreas()[h][v]->solveHorizontalForward();

            if (!lastH)
                subSolvers->getBoundsH()[h][v]->solveAlongForward();

            subSolvers->getCorners()[h][v]->solveForward();
        }
    }
    LOG4CXX_DEBUG(log, "solveIteration: Solve horizintal half-step in time, Backward");
    for (int h = subSolvers->sizeH(); h >= 0; h--)  // Backward
    {
        for (int v = 0; v < subSolvers->sizeV(); v++)
        {
            bool lastH = h == subSolvers->sizeH();
            bool lastV = v == subSolvers->sizeV();

            if (!lastV)
                subSolvers->getBoundsV()[h][v]->solveThroughBackward();

            if (!(h == 0 || lastV))
                subSolvers->getAreas()[h - 1][v]->solveHorizontalBackward();

            if (!lastH)
                subSolvers->getBoundsH()[h][v]->solveAlongBackward();

            subSolvers->getCorners()[h][v]->solveBackward();
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    //  Solve "vertical" half-step in time
    //
    LOG4CXX_DEBUG(log, "solveIteration: Solve vertical half-step in time, Forward");
    for (int v = 0; v < subSolvers->sizeV(); v++)   // Forward
    {
        for (int h = 0; h < subSolvers->sizeH(); h++)
        {
            bool lastH = h == subSolvers->sizeH();
            bool lastV = v == subSolvers->sizeV();

            if (!lastH)
                subSolvers->getBoundsH()[h][v]->solveThroughForward();

            if (!(lastH || lastV))
                subSolvers->getAreas()[h][v]->solveVerticalForward();

            if (!lastV)
                subSolvers->getBoundsV()[h][v]->solveAlongForward();

            subSolvers->getCorners()[h][v]->solveForward();
        }
    }
    LOG4CXX_DEBUG(log, "solveIteration: Solve vertical half-step in time, Backward");
    for (int v = subSolvers->sizeV(); v >= 0; v--)  // Backward
    {
        for (int h = 0; h < subSolvers->sizeH(); h++)
        {
            bool lastH = h == subSolvers->sizeH();
            bool lastV = v == subSolvers->sizeV();

            if (!lastH)
                subSolvers->getBoundsH()[h][v]->solveThroughBackward();

            if (!(lastH || v == 0))
                subSolvers->getAreas()[h][v - 1]->solveVerticalBackward();

            if (!lastV)
                subSolvers->getBoundsV()[h][v]->solveAlongBackward();

            subSolvers->getCorners()[h][v]->solveBackward();
        }
    }

    LOG4CXX_DEBUG(log, "solveIteration()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
