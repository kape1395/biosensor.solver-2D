#include "AreaSubSolver.hxx"
#include <bio/Exception.hxx>

/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::AreaSubSolver::AreaSubSolver(
    Solver *solver,
    int positionH,
    int positionV,
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
    BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer* fdAnalyzer
) : log(log4cxx::Logger::getLogger("libbiosensor-slv-fd::im2d::AreaSubSolver"))
{
    using BIO_XML_NS::model::s::Axis;
    using BIO_XML_NS::model::s::ConstantAxisPart;

    LOG4CXX_DEBUG(log, "AreaSubSolver()");
    this->solver = solver;
    this->positionH = positionH;
    this->positionV = positionV;

    ConstantAxisPart* axisPartH = dynamic_cast<ConstantAxisPart*> (fdAnalyzer->getAxisPartH(positionH));
    ConstantAxisPart* axisPartV = dynamic_cast<ConstantAxisPart*> (fdAnalyzer->getAxisPartV(positionV));
    if (axisPartH == 0 || axisPartV == 0)
    {
        LOG4CXX_ERROR(log, "Only axis parts of type ConstantAxisPart are now supported.");
        throw Exception("Invalid coonf.");
    }

    this->dataSizeH = axisPartH->stepCount() + 1;
    this->dataSizeV = axisPartV->stepCount() + 1;
    this->dataSizeS = structAnalyzer->getSubstances().size();

    this->stepSizeH = (structAnalyzer->getSymbol(axisPartH->to())->value() -
                       structAnalyzer->getSymbol(axisPartH->from())->value()) /
                      axisPartH->stepCount();

    this->stepSizeV = (structAnalyzer->getSymbol(axisPartV->to())->value() -
                       structAnalyzer->getSymbol(axisPartV->from())->value()) /
                      axisPartV->stepCount();

    this->startPositionH = structAnalyzer->getSymbol(axisPartH->from())->value();
    this->startPositionV = structAnalyzer->getSymbol(axisPartV->from())->value();

    this->layersInverted = false;

    data = new double***[dataSizeH];
    for (int h = 0; h < dataSizeH; h++)
    {
        data[h] = new double**[dataSizeV];
        for (int v = 0; v < dataSizeV; v++)
        {
            data[h][v] = new double*[dataSizeS];
            for (int s = 0; s < dataSizeS; s++)
            {
                data[h][v][s] = new double[5];

                // Apply initial conditions.
                data[h][v][s][0] = structAnalyzer->getInitialConcentration(
                                       s, positionH, positionH
                                   )->value();
            }
        }
    }


    if (structAnalyzer->isCoordinateSystemCartesian())
    {
        coordinateSystemIsCartesian = true;
        coordinateSystemIsCylindrical = false;
    }
    else if (structAnalyzer->isCoordinateSystemCylindrical())
    {
        coordinateSystemIsCartesian = false;
        coordinateSystemIsCylindrical = true;
    }
    else
    {
        throw Exception("Unsupported coordinate system");
    }


    // Collect information about diffusions
    D = new double[dataSizeS];
    for (int i = 0; i < dataSizeS; i++)
    {
        BIO_XML_NS::model::Symbol *sym = structAnalyzer->getDiffusion(i, positionH, positionV);
        D[i] = sym == 0 ? 0.0 : sym->value();
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::AreaSubSolver::~AreaSubSolver()
{
    delete [] D;

    LOG4CXX_DEBUG(log, "~AreaSubSolver()");
    for (int h = 0; h < dataSizeH; h++)
    {
        for (int v = 0; v < dataSizeV; v++)
        {
            for (int s = 0; s < dataSizeS; s++)
            {
                delete[] data[h][v][s];
            }
            delete[] data[h][v];
        }
        delete[] data[h];
    }
    delete[] data;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::AreaSubSolver::solveHorizontalForward()
{
    // This "solve method" is executed firstly, so we must invert layers.
    layersInverted = !layersInverted;

    double timeStep = this->solver->getTimeStep();
    int layerThis = layersInverted ? 2 : 0;
    int i = 0;
    for (int h = 1; h < dataSizeH - 1; h++) // dont calculate boundaries
    {
        double r = startPositionH + stepSizeH * h;
        double ***dataH = data[h];
        for (int v = 1; v < dataSizeV - 1; v++) // dont calculate boundaries
        {
            double **dataHV = dataH[v];
            for (int s = 0; s < dataSizeS; s++)
            {
                double *dataHVS = dataHV[s];

                // Calculate needed coefficients.
                double a = 0.0;
                double b = -2.0 / timeStep;     // b_T
                double c = 0.0;
                double f = -2.0 * dataHVS[layerThis] / timeStep;    // f_T

                // if diffusion?
                if (coordinateSystemIsCartesian)
                {
                    a = c = D[s] / (stepSizeH * stepSizeH);
                    b += - 2.0 * a;
                }
                else    // coordinateSystemIsCylindrical
                {
                    a = D[s] * (r - stepSizeH / 2) / (r * stepSizeH * stepSizeH);
                    b += -2.0 * D[s] / (stepSizeH * stepSizeH);
                    c = D[s] * (r + stepSizeH / 2) / (r * stepSizeH * stepSizeH);
                }
                // f_D is the same for cartesian and cylindrical coordinate systems.
                f += (- D[s] / (stepSizeV * stepSizeV)) * (
                         dataH[v+1][s][layerThis]
                         - 2.0 * dataHVS[layerThis]
                         + dataH[v-1][s][layerThis]
                     );

                // FIXME: Add reactions...

                // And now we are able to calculate layers:
                double tmp = a * data[h - 1][v][s][LAYER_P] + b;
                dataHVS[LAYER_P] = - c / tmp;
                dataHVS[LAYER_Q] = (f - a * data[h - 1][v][s][LAYER_Q]) / tmp;
            }
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::AreaSubSolver::solveHorizontalBackward()
{
    int i = 0;
    for (int h = dataSizeH - 2; h > 0; h--) // dont calculate boundaries
    {
        double ***dataH = data[h];
        for (int v = 1; v < dataSizeV - 1; v++) // dont calculate boundaries
        {
            double **dataHV = dataH[v];
            for (int s = 0; s < dataSizeS; s++)
            {
                double *dataHVS = dataHV[s];

                dataHVS[LAYER_INTERM] =
                    dataHVS[LAYER_P] *
                    data[h + 1][v][s][LAYER_INTERM] +
                    dataHVS[LAYER_Q];
            }
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::AreaSubSolver::solveVerticalForward()
{
    int i = 0;
    for (int h = 0; h < dataSizeH; h++)
    {
        double ***dataH = data[h];
        for (int v = 0; v < dataSizeV; v++)
        {
            double **dataHV = dataH[v];
            for (int s = 0; s < dataSizeS; s++)
            {
                double *dataHVS = dataHV[s];
                //dataHVS[0] += h + v - s;
                // FIXME: Do something.
            }
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::AreaSubSolver::solveVerticalBackward()
{
}


/* ************************************************************************** */
/* ************************************************************************** */
