#include "AreaSubSolver.hxx"


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::AreaSubSolver::AreaSubSolver(
int positionH,
        int positionV,
        BIO_CFG_NS::StructureAnalyzer* structAnalyzer
) : log(log4cxx::Logger::getLogger("libbiosensor-slv-fd::im2d::AreaSubSolver"))
{
    LOG4CXX_DEBUG(log, "AreaSubSolver()");
    this->positionH = positionH;
    this->positionV = positionV;
    this->dataSizeH = 200;  // FIXME: Get this from config.
    this->dataSizeV = 200;  // FIXME: Get this from config.
    this->dataSizeS = structAnalyzer->getSubstances().size();
    
    data = new double***[dataSizeH];
    for (int h = 0; h < dataSizeH; h++)
    {
        data[h] = new double**[dataSizeV];
        for (int v = 0; v < dataSizeV; v++)
        {
            data[h][v] = new double*[dataSizeS];
            for (int s = 0; s < dataSizeS; s++)
            {
                data[h][v][s] = new double[4];
                // Apply initial conditions.
            }
        }
    }
    
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::AreaSubSolver::~AreaSubSolver()
{
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
void BIO_SLV_FD_IM2D_NS::AreaSubSolver::solveFirstHalfStep()
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
                dataHVS[0] += h + v - s;
                // FIXME: Do something.
            }
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::AreaSubSolver::solveSecondHalfStep()
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
                dataHVS[0] += h + v - s;
                // FIXME: Do something.
            }
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
