#include "DataModel.hxx"
#include <bio/dm/CompositeSegmentSplit.hxx>


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::DataModel::DataModel(
    Solver* solver,
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
    BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer* fdAnalyzer
) :
        BIO_DM_NS::IComposite2D(
            solver->getSubSolvers()->sizeH(),
            solver->getSubSolvers()->sizeV()
        )
{
    this->solver = solver;
    this->structAnalyzer = structAnalyzer;

    areaCountH = solver->getSubSolvers()->sizeH();
    areaCountV = solver->getSubSolvers()->sizeV();
    areaRangesH = new int[areaCountH + 1];
    areaRangesV = new int[areaCountV + 1];


    int accumulatedPointCount = 0;
    std::vector<BIO_DM_NS::ISegmentSplit*> subSegmentSplitsH;
    for (int h = 0; h < areaCountH; h++)
    {
        BIO_DM_NS::ISegmentSplit* segment = fdAnalyzer->getAxisPartSegmentSplitH(h);
        areaRangesH[h] = accumulatedPointCount;
        accumulatedPointCount += segment->getStepCount();
        subSegmentSplitsH.push_back(segment);
    }
    areaRangesH[areaCountH] = accumulatedPointCount;


    accumulatedPointCount = 0;
    std::vector<BIO_DM_NS::ISegmentSplit*> subSegmentSplitsV;
    for (int v = 0; v < areaCountV; v++)
    {
        BIO_DM_NS::ISegmentSplit* segment = fdAnalyzer->getAxisPartSegmentSplitV(v);
        areaRangesV[v] = accumulatedPointCount;
        accumulatedPointCount += segment->getStepCount();
        subSegmentSplitsV.push_back(segment);
    }
    areaRangesV[areaCountV] = accumulatedPointCount;


    segmentSplitH = new BIO_DM_NS::CompositeSegmentSplit(subSegmentSplitsH);
    segmentSplitV = new BIO_DM_NS::CompositeSegmentSplit(subSegmentSplitsV);
    pointCountH = segmentSplitH->getPointCount();
    pointCountV = segmentSplitV->getPointCount();
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::DataModel::~DataModel()
{
    delete [] areaRangesH;
    delete [] areaRangesV;
    delete segmentSplitH;
    delete segmentSplitV;
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
BIO_DM_NS::ISegmentSplit* BIO_SLV_FD_IM2D_NS::DataModel::getPointPositionsH()
{
    return segmentSplitH;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_DM_NS::ISegmentSplit* BIO_SLV_FD_IM2D_NS::DataModel::getPointPositionsV()
{
    return segmentSplitV;
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
    sizeH = dataModel->getPointPositionsH()->getPointCount();
    sizeV = dataModel->getPointPositionsV()->getPointCount();
    currentH = 0;
    currentV = 0;
    currentAreaH = 0;
    currentAreaV = 0;
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
        currentAreaH = dataModel->areaCountH;
        currentAreaH > 0 && dataModel->areaRangesH[currentAreaH] > currentH;
        currentAreaH--
    );
    for (
        currentAreaV = dataModel->areaCountV;
        currentAreaV > 0 && dataModel->areaRangesV[currentAreaV] > currentV;
        currentAreaV--
    );
    currentOnBoundH = dataModel->areaRangesH[currentAreaH] == currentH;
    currentOnBoundV = dataModel->areaRangesV[currentAreaV] == currentV;

    return this;
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_SLV_FD_IM2D_NS::DataModel::Cursor::operator[] (int substanceNr)
{
    if (!currentOnBoundH && !currentOnBoundV)
    {
        return  dataModel->solver->getSubSolvers()->getArea(
                    currentAreaH,
                    currentAreaV
                )->getConcentration(
                    currentH - dataModel->areaRangesH[currentAreaH],
                    currentV - dataModel->areaRangesV[currentAreaV],
                    substanceNr
                );
    }
    else if (currentOnBoundH && currentOnBoundV)
    {
        return dataModel->solver->getSubSolvers()->getCorner(
                   currentAreaH,
                   currentAreaV
               )->getConcentration(
                   substanceNr
               );
    }
    else if (currentOnBoundH)
    {
        return dataModel->solver->getSubSolvers()->getBoundV(
                   currentAreaH,
                   currentAreaV
               ) ->getConcentration(
                   currentV - dataModel->areaRangesV[currentAreaV],
                   substanceNr
               );
    }
    else if (currentOnBoundV)
    {
        return dataModel->solver->getSubSolvers()->getBoundH(
                   currentAreaH,
                   currentAreaV
               ) ->getConcentration(
                   currentH - dataModel->areaRangesH[currentAreaH],
                   substanceNr
               );
    }
    return 0;   //  Control will never reach this statement.
}


/* ************************************************************************** */
/* ************************************************************************** */
