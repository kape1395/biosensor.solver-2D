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
#include "DataModel.hxx"
#include <bio/dm/CompositeSegmentSplit.hxx>
#include <bio/dm/IConcentrations.hxx>
#include <bio/Logging.hxx>
#include <bio/Exception.hxx>
#define LOGGER "libbiosensor-slv-fd::im2d::DataModel: "


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
void BIO_SLV_FD_IM2D_NS::DataModel::setState(BIO_DM_NS::IDataModel *source)
{
    LOG_DEBUG(LOGGER << "setState...");
    BIO_DM_NS::IGrid2D* srcGrid = dynamic_cast<BIO_DM_NS::IGrid2D*>(source);

    if (srcGrid == 0)
        throw BIO_NS::Exception("This data model supports IGrid2D only...");


    if (srcGrid->getSubstanceCount() != getSubstanceCount() ||
            srcGrid->getPointPositionsH()->getPointCount() != getPointPositionsH()->getPointCount() ||
            srcGrid->getPointPositionsV()->getPointCount() != getPointPositionsV()->getPointCount())
        throw BIO_NS::Exception("Size and substances should match when setting the state for the IM2D data model.");

    BIO_DM_NS::ICursor2D *srcCursor = srcGrid->newGridCursor();
    Cursor *dstCursor = dynamic_cast<BIO_SLV_FD_IM2D_NS::DataModel::Cursor*>(this->newGridCursor());

    srcCursor->colStart();
    srcCursor->rowStart();
    dstCursor->colStart();
    dstCursor->rowStart();
    for (; srcCursor->isValid(); srcCursor->down(), dstCursor->down())
    {
        for (; srcCursor->isValid(); srcCursor->right(), dstCursor->right())
        {
            dstCursor->setConcentrations(srcCursor->getConcentrations());
        }
        srcCursor->rowStart();
        dstCursor->rowStart();
    }

    delete srcCursor;
    delete dstCursor;

    LOG_DEBUG(LOGGER << "setState... Done");
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
void BIO_SLV_FD_IM2D_NS::DataModel::Cursor::setConcentrations(BIO_DM_NS::IConcentrations* source)
{
    this->getConcentrations();  // only calculates current position correctly and returns THIS.
    for (int s = 0; s < dataModel->getSubstanceCount(); s++)
    {
        setConcentration(s, source->getConcentration(s));
    }
    /**
    Solver::SplittedSolver* ss = dataModel->solver->getSubSolvers();

    if (!currentOnBoundH && !currentOnBoundV)
    {
        int localH = currentH - dataModel->areaRangesH[currentAreaH];
        int localV = currentV - dataModel->areaRangesV[currentAreaV];
        AreaSubSolver* area = ss->getArea(currentAreaH, currentAreaV);

        for (int s = 0; s < dataModel->getSubstanceCount(); s++)
        {
            area->setConcentration(localH, localV, s, source->getConcentration(s));
        }
    }
    else if (currentOnBoundH && currentOnBoundV)
    {
        CornerSubSolver* corner = ss->getCorner(currentAreaH, currentAreaV);

        for (int s = 0; s < dataModel->getSubstanceCount(); s++)
        {
            corner->setConcentration(s, source->getConcentration(s));
        }
    }
    else if (currentOnBoundH)
    {
        int local = currentV - dataModel->areaRangesV[currentAreaV];
        BoundSubSolver* bound = ss->getBoundV(currentAreaH, currentAreaV);

        for (int s = 0; s < dataModel->getSubstanceCount(); s++)
        {
            bound->setConcentration(local, s, source->getConcentration(s));
        }
    }
    else if (currentOnBoundV)
    {
        int local = currentH - dataModel->areaRangesH[currentAreaH];
        BoundSubSolver* bound = ss->getBoundH(currentAreaH, currentAreaV);

        for (int s = 0; s < dataModel->getSubstanceCount(); s++)
        {
            bound->setConcentration(local, s, source->getConcentration(s));
        }
    }
     */
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
void BIO_SLV_FD_IM2D_NS::DataModel::Cursor::setConcentration(int substanceNr, double concentration)
{
    if (!currentOnBoundH && !currentOnBoundV)
    {
        return  dataModel->solver->getSubSolvers()->getArea(
                    currentAreaH,
                    currentAreaV
                )->setConcentration(
                    currentH - dataModel->areaRangesH[currentAreaH],
                    currentV - dataModel->areaRangesV[currentAreaV],
                    substanceNr, concentration
                );
    }
    else if (currentOnBoundH && currentOnBoundV)
    {
        return dataModel->solver->getSubSolvers()->getCorner(
                   currentAreaH,
                   currentAreaV
               )->setConcentration(
                   substanceNr, concentration
               );
    }
    else if (currentOnBoundH)
    {
        return dataModel->solver->getSubSolvers()->getBoundV(
                   currentAreaH,
                   currentAreaV
               ) ->setConcentration(
                   currentV - dataModel->areaRangesV[currentAreaV],
                   substanceNr, concentration
               );
    }
    else if (currentOnBoundV)
    {
        return dataModel->solver->getSubSolvers()->getBoundH(
                   currentAreaH,
                   currentAreaV
               ) ->setConcentration(
                   currentH - dataModel->areaRangesH[currentAreaH],
                   substanceNr, concentration
               );
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
