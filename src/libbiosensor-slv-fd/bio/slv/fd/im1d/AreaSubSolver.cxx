/*
 * Copyright 2011 Karolis Petrauskas
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#include "AreaSubSolver.hxx"
#include <bio/Logging.hxx>
#include "../im2d/ReactionMacro.hxx"
#define LOGGER "libbiosensor-slv-fd::im2d::AreaSubSolver: "

/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM1D_NS::AreaSubSolver::AreaSubSolver(
    BIO_SLV_FD_IM2D_NS::Solver *solver,
    int positionH,
    int positionV,
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
    BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer* fdAnalyzer
) :
    BIO_SLV_FD_IM2D_NS::AreaSubSolver::AreaSubSolver(
        solver, positionH, positionV, structAnalyzer, fdAnalyzer
    )
{
    // Nothiong to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM1D_NS::AreaSubSolver::~AreaSubSolver()
{
    // Nothiong to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
double**** BIO_SLV_FD_IM1D_NS::AreaSubSolver::createData()
{
    LOG_DEBUG(LOGGER << "BIO_SLV_FD_IM1D_NS::AreaSubSolver::createData()...");

    double**** data = new double***[getPointCountH()];

    // Create one column
    data[0] = new double**[getPointCountV()];
    for (int v = 0; v < getPointCountV(); v++)
    {
        data[0][v] = createDataPoint();
    }

    //  Mirror ir to all other columns.
    for (int h = 1; h < getPointCountH(); h++)
    {
        data[h] = data[0];
    }

    LOG_DEBUG(LOGGER << "BIO_SLV_FD_IM1D_NS::AreaSubSolver::createData()... Done");
    return data;
}

void BIO_SLV_FD_IM1D_NS::AreaSubSolver::deleteData(double**** data)
{
    for (int v = 0; v < getPointCountV(); v++)
    {
        deleteDataPoint(data[0][v]);
    }
    delete[] data[0];
    delete[] data;
}


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  NOTE: Almost copy of IM2D.
 */
void BIO_SLV_FD_IM1D_NS::AreaSubSolver::solveVerticalForward()
{
    LOG_TRACE(LOGGER << "solveVerticalForward()...");
    if (dataSizeS == 0)
    {
        // Nothing to solve...
        return;
    }

    // Make bject properties as local variables, for performance...
    static const int LAYER_P = this->LAYER_P;
    static const int LAYER_Q = this->LAYER_Q;
    double ****data = this->data;
    int dataSizeV = this->dataSizeV;
    int dataSizeS = this->dataSizeS;
    double stepSizeV = this->stepSizeV;
    double *D_v = this->D_v;
    ReactionMMPart **reactionsMM = this->reactionsMM;
    int *reactionsMMPartCounts = this->reactionsMMPartCounts;
    ReactionROPart **reactionsRO = this->reactionsRO;
    int *reactionsROPartCounts = this->reactionsROPartCounts;



    // This "solve method" is executed firstly, so we must invert layers.
    dataLayersInverted = !dataLayersInverted;
    targetLayerIndex = getCurrentLayerIndex();
    int layerPrev = this->getPreviousLayerIndex();

    dumpData(std::cout, false, "solveVerticalForward, before calculations");

    double exprTimeStep = -1.0 / this->solver->getTimeStep();
    double exprStepSizeV = 1.0 / (stepSizeV * stepSizeV);

    int h = 1;  // Only solve midle column.
    double ***dataH = data[h];
    for (int v = 1; v < dataSizeV - 1; v++) // dont calculate boundaries
    {
        double **dataHV = dataH[v];
        for (int s = 0; s < dataSizeS; s++)
        {
            double *dataHVS = dataHV[s];
            double *dataHVS_T = dataH[v - 1][s];  //  Should ir be "Bottom"?
            double D_vs = D_v[s];

            //
            //  Time part of the coefficients (b+=b_T, f+=f_T)
            //
            double a = 0.0;
            double b = exprTimeStep;                         // b_T
            double c = 0.0;
            double f = exprTimeStep * dataHVS[layerPrev];    // f_T


            //
            //  Diffusion part (a=a_D, b+=b_D, c=c_D, f+=f_D)
            //
            a = c = D_vs * exprStepSizeV;
            b += - 2.0 * a;
            // f +=0 ; // f_D

            //
            //  Reaction part (f+=f_R)
            //
            double f_R = 0.0;
            AREA_SUBSOLVER_REACTION_MACRO(
                f_R, layerPrev, dataHV,
                reactionsMM[s], reactionsMMPartCounts[s],
                reactionsRO[s], reactionsROPartCounts[s]
            )
            f += f_R;       // f_R

            //
            // And now we are able to calculate layers:
            //
            double denominator = 1.0 / (a * dataHVS_T[LAYER_P] + b);
            dataHVS[LAYER_P] = - c * denominator;
            dataHVS[LAYER_Q] = (f - a * dataHVS_T[LAYER_Q]) * denominator;
        }
    }

    dumpData(std::cout, false, "solveVerticalForward, after calculations");

    LOG_TRACE(LOGGER << "solveVerticalForward()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  NOTE: Almost copy of IM2D.
 */
void BIO_SLV_FD_IM1D_NS::AreaSubSolver::solveVerticalBackward()
{
    LOG_TRACE(LOGGER << "solveVerticalBackward()...");
    if (dataSizeS == 0)
    {
        // Nothing to solve...
        return;
    }

    static const int LAYER_P = this->LAYER_P;
    static const int LAYER_Q = this->LAYER_Q;
    double ****data = this->data;
    int dataSizeV = this->dataSizeV;
    int dataSizeS = this->dataSizeS;

    int layerCurrent = getCurrentLayerIndex();

    int h = 1;  // Only solve the middle column.
    double ***dataH = data[h];
    for (int v = dataSizeV - 2; v > 0; v--) // dont calculate boundaries
    {
        double **dataHV = dataH[v];
        for (int s = 0; s < dataSizeS; s++)
        {
            double *dataHVS = dataHV[s];

            dataHVS[layerCurrent] =
                dataHVS[LAYER_P] *
                dataH[v + 1][s][layerCurrent] +
                dataHVS[LAYER_Q];
        }
    }

    dumpData(std::cout, true, "solveVerticalBackward, after calculations");
    LOG_TRACE(LOGGER << "solveVerticalBackward()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
