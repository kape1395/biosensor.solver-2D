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
    areaRangesH = new unsigned[areaCountH + 1];
    areaRangesV = new unsigned[areaCountV + 1];


    for (unsigned h = 0; h <= areaCountH; h++)
    {
        for (unsigned v = 0; v <= areaCountV; v++)
        {
            bool lastH = (h == areaCountH);
            bool lastV = (v == areaCountV);

            if (!lastH && !lastV)
                this->getArea(h, v) = solver->getSubSolvers()->getArea(h, v);

            if (!lastH)
                this->getBoundH(h, v) = solver->getSubSolvers()->getBoundH(h, v);

            if (!lastV)
                this->getBoundV(h, v) = solver->getSubSolvers()->getBoundV(h, v);

            this->getCorner(h, v) = solver->getSubSolvers()->getCorner(h, v);
        }
    }



    int accumulatedPointCount = 0;
    std::vector<BIO_DM_NS::ISegmentSplit*> subSegmentSplitsH;
    for (unsigned h = 0; h < areaCountH; h++)
    {
        BIO_DM_NS::ISegmentSplit* segment = fdAnalyzer->getAxisPartSegmentSplitH(h);
        areaRangesH[h] = accumulatedPointCount;
        accumulatedPointCount += segment->getStepCount();
        subSegmentSplitsH.push_back(segment);
    }
    areaRangesH[areaCountH] = accumulatedPointCount;


    accumulatedPointCount = 0;
    std::vector<BIO_DM_NS::ISegmentSplit*> subSegmentSplitsV;
    for (unsigned v = 0; v < areaCountV; v++)
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
) :
        BIO_DM_NS::AbstractCursor2D(
            dataModel->getPointPositionsH()->getPointCount(),
            dataModel->getPointPositionsV()->getPointCount()
        )
{
    this->dataModel = dataModel;
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
double BIO_SLV_FD_IM2D_NS::DataModel::Cursor::getConcentration(int substanceNr)
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
