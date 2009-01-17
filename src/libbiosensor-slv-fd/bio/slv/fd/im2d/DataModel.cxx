#include "DataModel.hxx"


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::DataModel::DataModel(
    Solver* solver,
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
    BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer* fdAnalyzer
)
{
    this->solver = solver;
    this->structAnalyzer = structAnalyzer;

    areaCountH = solver->getSubSolvers()->sizeH();
    areaCountV = solver->getSubSolvers()->sizeV();
    areaRangesH = new int[areaCountH];
    areaRangesV = new int[areaCountV];

    int accumulatedPointCount = 0;
    for (int h = 0; h < areaCountH; h++)
    {
        areaRangesH[h] = accumulatedPointCount;
        accumulatedPointCount += solver->getSubSolvers()->getArea(h, 0)->getPointCountH() - 1;
    }
    pointCountH = accumulatedPointCount + 1;

    accumulatedPointCount = 0;
    for (int v = 0; v < areaCountV; v++)
    {
        areaRangesV[v] = accumulatedPointCount;
        accumulatedPointCount += solver->getSubSolvers()->getArea(0, v)->getPointCountV() - 1;
    }
    pointCountV = accumulatedPointCount + 1;

}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::DataModel::~DataModel()
{
    delete [] areaRangesH;
    delete [] areaRangesV;
}


/* ************************************************************************** */
/* ************************************************************************** */
int BIO_SLV_FD_IM2D_NS::DataModel::getSubstanceCount()
{
    return structAnalyzer->getSubstances().size();
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_XML_NS::model::Substance* BIO_SLV_FD_IM2D_NS::DataModel::getSubstanceConf(int index)
{
    return structAnalyzer->getSubstances()[index];
}


/* ************************************************************************** */
/* ************************************************************************** */
int BIO_SLV_FD_IM2D_NS::DataModel::getPointCountH()
{
    return pointCountH;
}


/* ************************************************************************** */
/* ************************************************************************** */
int BIO_SLV_FD_IM2D_NS::DataModel::getPointCountV()
{
    return pointCountV;
}


/* ************************************************************************** */
/* ************************************************************************** */
double* BIO_SLV_FD_IM2D_NS::DataModel::getPointPositionsH()
{
    // TODO: Implement
}


/* ************************************************************************** */
/* ************************************************************************** */
double* BIO_SLV_FD_IM2D_NS::DataModel::getPointPositionsV()
{
    // TODO: Implement
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_DM_NS::ICursor2D* BIO_SLV_FD_IM2D_NS::DataModel::newGridCursor()
{
    return new Cursor(this);
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::DataModel::Cursor::Cursor(
    DataModel* dataModel
)
{
    this->dataModel = dataModel;
    sizeH = dataModel->getPointCountH();
    sizeV = dataModel->getPointCountV();
    currentH = 0;
    currentV = 0;
    currentAreaH = 0;
    currentAreaV = 0;
    currentArea = dataModel->solver->getSubSolvers()->getArea(currentAreaH, currentAreaV);
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::DataModel::Cursor::~Cursor()
{
    // Nothing to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::DataModel::Cursor::left()
{
    --currentH;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::DataModel::Cursor::right()
{
    currentH++;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::DataModel::Cursor::top()
{
    currentV--;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::DataModel::Cursor::down()
{
    currentV++;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::DataModel::Cursor::rowStart()
{
    currentH = 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::DataModel::Cursor::rowEnd()
{
    currentH = sizeH - 1;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::DataModel::Cursor::colStart()
{
    currentV = 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::DataModel::Cursor::colEnd()
{
    currentV = sizeV - 1;
}


/* ************************************************************************** */
/* ************************************************************************** */
bool BIO_SLV_FD_IM2D_NS::DataModel::Cursor::isValid()
{
    return currentH >= 0 && currentH < sizeH && currentV >= 0 && currentV < sizeV;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_DM_NS::IConcentrations* BIO_SLV_FD_IM2D_NS::DataModel::Cursor::getConcentrations()
{
    if (!isValid())
    {
        return 0;
    }

    //  Find current area
    for (
        currentAreaH = dataModel->areaCountH - 1;
        currentAreaH >= 0 && dataModel->areaRangesH[currentAreaH] > currentH;
        currentAreaH--
    );
    for (
        currentAreaV = dataModel->areaCountV - 1;
        currentAreaV >= 0 && dataModel->areaRangesV[currentAreaV] > currentV;
        currentAreaV--
    );

    return this;
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_SLV_FD_IM2D_NS::DataModel::Cursor::operator[] (int substanceNr)
{
    return currentArea->getConcentration(
               currentH - dataModel->areaRangesH[currentAreaH],
               currentV - dataModel->areaRangesV[currentAreaV],
               substanceNr
           );
}


/* ************************************************************************** */
/* ************************************************************************** */
