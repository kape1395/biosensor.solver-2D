#include "AreaSubSolver.hxx"
#include <bio/Exception.hxx>

/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::AreaSubSolver::AreaSubSolver(
    int positionH,
    int positionV,
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
    BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer* fdAnalyzer
) : log(log4cxx::Logger::getLogger("libbiosensor-slv-fd::im2d::AreaSubSolver"))
{
    using BIO_XML_NS::model::s::Axis;
    using BIO_XML_NS::model::s::ConstantAxisPart;

    LOG4CXX_DEBUG(log, "AreaSubSolver()");
    this->positionH = positionH;
    this->positionV = positionV;

    ConstantAxisPart* axisPartH = dynamic_cast<ConstantAxisPart*>(fdAnalyzer->getAxisPartH(positionH));
    ConstantAxisPart* axisPartV = dynamic_cast<ConstantAxisPart*>(fdAnalyzer->getAxisPartV(positionV));
    if (axisPartH == 0 || axisPartV == 0)
    {
        LOG4CXX_ERROR(log, "Only axis parts of type ConstantAxisPart are now supported.");
        throw Exception("Invalid coonf.");
    }

    this->dataSizeH = axisPartH->stepCount();
    this->dataSizeV = axisPartH->stepCount();
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
void BIO_SLV_FD_IM2D_NS::AreaSubSolver::solveHorizontal()
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
void BIO_SLV_FD_IM2D_NS::AreaSubSolver::solveVertical()
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
