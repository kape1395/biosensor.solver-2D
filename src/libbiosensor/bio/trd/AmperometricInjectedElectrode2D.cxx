#include "AmperometricInjectedElectrode2D.hxx"
#include "../Logging.hxx"
#include "../Exception.hxx"
#include "../dm/ConstantSegmentSplit.hxx"
#include "../slv/IIterativeSolver.hxx"
#define LOGGER "libbiosensor::AmperometricInjectedElectrode2D: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::AmperometricInjectedElectrode2D::AmperometricInjectedElectrode2D(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::MediumName& mediumName,
    BIO_XML_MODEL_NS::SubstanceName& substanceName,
    BIO_XML_MODEL_NS::SymbolName& reactionSpeedSymbolName
)
{
    if (!dynamic_cast<BIO_SLV_NS::IIterativeSolver*>(solver))
        throw Exception("InjectedElectrode: Solver must implement IIterativeSolver.");

    if (!(dataModel = dynamic_cast<BIO_DM_NS::IComposite2D*>(solver->getData())))
        throw Exception("InjectedElectrode: DataModel must implement IComposite2D.");


    this->solver = solver;
    this->structAnalyzer = new BIO_CFG_NS::StructureAnalyzer(solver->getConfig());
    this->mediumName = mediumName;
    this->substanceName = substanceName;
    this->substanceIndex = structAnalyzer->getSubstanceIndex(substanceName);
    this->reactionSpeed = structAnalyzer->getSymbol(reactionSpeedSymbolName)->value();


    for (int h = 0; h < dataModel->sizeH(); h++)
    {
        for (int v = 0; v < dataModel->sizeV(); v++)
        {
            if (structAnalyzer->getMediumName(h, v) && (structAnalyzer->getMediumName(h, v)->compare(mediumName) == 0))
            {
                std::vector<int> localSubstIndexes = structAnalyzer->getSubstanceIndexesInArea(h, v);
                bool substanceFound = false;
                for (unsigned i = 0; i < localSubstIndexes.size(); i++)
                {
                    if (localSubstIndexes[i] == substanceIndex)
                    {
                        substanceFound = true;
                        break;
                    }
                }
                if (!substanceFound)
                    throw Exception("InjectedElectrode: substance not exists in the specified medium.");

                if (!dynamic_cast<BIO_DM_NS::ConstantSegmentSplit*>(dataModel->getArea(h, v)->getPointPositionsH()))
                    throw Exception("InjectedElectrode: only grid with constant steps is supported");

                if (!dynamic_cast<BIO_DM_NS::ConstantSegmentSplit*>(dataModel->getArea(h, v)->getPointPositionsV()))
                    throw Exception("InjectedElectrode: only grid with constant steps is supported");

                areas.push_back(dataModel->getArea(h, v));
            } // if name
        }
    }
    if (areas.size() == 0)
        throw Exception("InjectedElectrode: No areas were found with specified medium name.");

    this->calculatedOutput = 0.0;
    this->calculatedOutputForStep = -1;
}




/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::AmperometricInjectedElectrode2D::~AmperometricInjectedElectrode2D()
{
    delete structAnalyzer;
    areas.clear();
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_TRD_NS::AmperometricInjectedElectrode2D::getOutput()
{
    BIO_SLV_NS::IIterativeSolver* iterative = dynamic_cast<BIO_SLV_NS::IIterativeSolver*>(solver);
    if (calculatedOutputForStep == iterative->getSolvedIterationCount())
    {
        return calculatedOutput;
    }

    LOG_DEBUG(LOGGER << "getOutput()...");

    double integralValue = 0.0;
    for (unsigned i = 0; i < areas.size(); i++)
    {
        integralValue += integrateArea(areas[i]);
    }

    integralValue *= CONST_F * CONST_n_e * reactionSpeed;

    //
    //  Divide by surface.
    //
    if (structAnalyzer->isCoordinateSystemCylindrical())
    {
        //
        //   integrate by angle (\fi) and divide by an area of a circle:
        //      ((2 \pi) / (\pi r^2))
        //
        double cellRadius = structAnalyzer->getPointsH()[structAnalyzer->getPointsH().size() - 1]->value();
        integralValue *= 2.0 / (cellRadius * cellRadius);
    }
    else
    {
        //
        //  We are integrated all in one line, now just divide all by it`s lenght.
        //
        double areaWidth = structAnalyzer->getPointsH()[structAnalyzer->getPointsH().size() - 1]->value();
        integralValue /= areaWidth;
    }



    calculatedOutput = integralValue;
    calculatedOutputForStep = iterative->getSolvedIterationCount();

    LOG_DEBUG(LOGGER << "getOutput()... Done, result=" << integralValue);

    return integralValue;
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_TRD_NS::AmperometricInjectedElectrode2D::integrateArea(BIO_DM_NS::IGrid2D* area)
{
    LOG_DEBUG(LOGGER << "integrateArea()...");

    BIO_DM_NS::ICursor2D* cursor = area->newGridCursor();
    double stepH = area->getPointPositionsH()->getStepSize(0);  // NOTE: Only valid constant step segment
    double stepV = area->getPointPositionsV()->getStepSize(0);  // NOTE: Only valid constant step segment
    int pointCountH = area->getPointPositionsH()->getPointCount();
    int pointCountV = area->getPointPositionsV()->getPointCount();
    int h;
    int v;
    double sum = 0.0;
    for (cursor->colStart(), v = 0; cursor->rowStart(), cursor->isValid(); cursor->down(), v++)
    {
        for (h = 0; cursor->isValid(); cursor->right(), h++)
        {
            double coefficient = 1.0;

            if ((h == 0) || (h == pointCountH - 1))
                coefficient /= 2.0;

            if ((v == 0) || (v == pointCountV - 1))
                coefficient /= 2.0;

            if (structAnalyzer->isCoordinateSystemCylindrical())
                coefficient *= area->getPointPositionsH()->getPointPosition(h);

            sum += coefficient
                   * (*cursor->getConcentrations())[substanceIndex]
                   * stepH * stepV;

            LOG_TRACE(LOGGER << "integrateArea:"
                      << "\th=" << h
                      << "\tv=" << v
                      << "\tS=" << (*cursor->getConcentrations())[substanceIndex]
                      << "\tC=" << coefficient
                     );
        }
    }

    delete cursor;

    LOG_DEBUG(LOGGER << "integrateArea()... Done, result=" << sum);
    return sum;
}


/* ************************************************************************** */
/* ************************************************************************** */
