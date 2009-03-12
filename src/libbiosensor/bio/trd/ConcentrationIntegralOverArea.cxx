#include "ConcentrationIntegralOverArea.hxx"
#include "../Logging.hxx"
#include "../Exception.hxx"
#include "../dm/IComposite2D.hxx"
#include "../dm/ConstantSegmentSplit.hxx"
#include "../slv/IIterativeSolver.hxx"
#include <cmath>
#define LOGGER "libbiosensor::ConcentrationIntegralOverArea: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::ConcentrationIntegralOverArea::ConcentrationIntegralOverArea(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::MediumName& mediumName,
    BIO_XML_MODEL_NS::SubstanceName& substanceName,
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer
) :
    CONST_PI(std::atan2(0, -1))
{
    BIO_DM_NS::IComposite2D* dataModel = 0;

    if (!(dataModel = dynamic_cast<BIO_DM_NS::IComposite2D*>(solver->getData())))
        throw Exception("ConcentrationIntegralOverArea: DataModel must implement IComposite2D.");

    this->structAnalyzer = structAnalyzer;
    this->substanceName = substanceName;
    this->substanceIndex = structAnalyzer->getSubstanceIndex(substanceName);


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
                    throw Exception("ConcentrationIntegralOverArea: substance not exists in the specified medium.");

                if (!dynamic_cast<BIO_DM_NS::ConstantSegmentSplit*>(dataModel->getArea(h, v)->getPointPositionsH()))
                    throw Exception("ConcentrationIntegralOverArea: only grid with constant steps is supported");

                if (!dynamic_cast<BIO_DM_NS::ConstantSegmentSplit*>(dataModel->getArea(h, v)->getPointPositionsV()))
                    throw Exception("ConcentrationIntegralOverArea: only grid with constant steps is supported");

                areas.push_back(dataModel->getArea(h, v));
            } // if name
        }
    }
    if (areas.size() == 0)
        throw Exception("ConcentrationIntegralOverArea: No areas were found with specified medium name.");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::ConcentrationIntegralOverArea::ConcentrationIntegralOverArea(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::SubstanceName& substanceName,
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer
) :
    CONST_PI(std::atan2(0, -1))
{
    BIO_DM_NS::IComposite2D* dataModel = 0;

    if (!(dataModel = dynamic_cast<BIO_DM_NS::IComposite2D*>(solver->getData())))
        throw Exception("ConcentrationIntegralOverArea: DataModel must implement IComposite2D.");

    this->structAnalyzer = structAnalyzer;
    this->substanceName = substanceName;
    this->substanceIndex = structAnalyzer->getSubstanceIndex(substanceName);


    for (int h = 0; h < dataModel->sizeH(); h++)
    {
        for (int v = 0; v < dataModel->sizeV(); v++)
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
            if (substanceFound)
            {
                if (!dynamic_cast<BIO_DM_NS::ConstantSegmentSplit*>(dataModel->getArea(h, v)->getPointPositionsH()))
                    throw Exception("ConcentrationIntegralOverArea: only grid with constant steps is supported");

                if (!dynamic_cast<BIO_DM_NS::ConstantSegmentSplit*>(dataModel->getArea(h, v)->getPointPositionsV()))
                    throw Exception("ConcentrationIntegralOverArea: only grid with constant steps is supported");

                areas.push_back(dataModel->getArea(h, v));
            }
        }
    }
    if (areas.size() == 0)
        throw Exception("ConcentrationIntegralOverArea: No areas were found with specified medium name.");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::ConcentrationIntegralOverArea::~ConcentrationIntegralOverArea()
{
    areas.clear();
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_TRD_NS::ConcentrationIntegralOverArea::integrate()
{
    LOG_TRACE(LOGGER << "integrate()...");

    double integralValue = 0.0;
    for (unsigned i = 0; i < areas.size(); i++)
    {
        integralValue += integrateSubArea(areas[i]);
    }

    LOG_TRACE(LOGGER << "getOutput()... Done, result=" << integralValue);
    return integralValue;
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_TRD_NS::ConcentrationIntegralOverArea::integrateSubArea(BIO_DM_NS::IGrid2D* area)
{
    LOG_TRACE(LOGGER << "integrateArea()...");

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
                   * cursor->getConcentrations()->getConcentration(substanceIndex)
                   * stepH * stepV;

            LOG_TRACE(LOGGER << "integrateArea:"
                      << "\th=" << h
                      << "\tv=" << v
                      << "\tS=" << cursor->getConcentrations()->getConcentration(substanceIndex)
                      << "\tC=" << coefficient
                     );
        }
    }

    delete cursor;

    LOG_TRACE(LOGGER << "integrateArea()... Done, result=" << sum);
    return sum;
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_TRD_NS::ConcentrationIntegralOverArea::getArea()
{
    //  FIXME: Coordinate system must be taken into account.
    LOG_TRACE(LOGGER << "getArea()...");

    double area = 0.0;
    for (unsigned i = 0; i < areas.size(); i++)
    {
        area += areas[i]->getPointPositionsH()->getLength() * areas[i]->getPointPositionsV()->getLength();
    }

    LOG_TRACE(LOGGER << "getArea()... Done, result=" << area);
    return area;
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_TRD_NS::ConcentrationIntegralOverArea::getVolume()
{
    LOG_TRACE(LOGGER << "getVolume()...");
    
    double volume = 0.0;
    for (unsigned i = 0; i < areas.size(); i++)
    {
        if (structAnalyzer->isCoordinateSystemCylindrical())
        {
            double r1 = areas[i]->getPointPositionsH()->getStartPosition();
            double r2 = areas[i]->getPointPositionsH()->getLength() + r1;
            double z  = areas[i]->getPointPositionsV()->getLength();
            volume += CONST_PI * z * (r2 * r2 - r1 * r1);
        }
        else if (structAnalyzer->isCoordinateSystemCartesian())
        {
            double x = areas[i]->getPointPositionsH()->getLength();
            double y = areas[i]->getPointPositionsV()->getLength();
            volume += x * y;
        }
        else
        {
            throw BIO_NS::Exception("ConcentrationIntegralOverArea::getVolume: Unsupported coordinate system");
        }
    }

    LOG_TRACE(LOGGER << "getVolume()... Done, result=" << volume);
    return volume;
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_TRD_NS::ConcentrationIntegralOverArea::integrateOverVolume()
{
    if (structAnalyzer->isCoordinateSystemCylindrical())
    {
        return integrate() * 2.0 * CONST_PI;
    }
    else if (structAnalyzer->isCoordinateSystemCartesian())
    {
        return integrate();
    }
    else
    {
        throw BIO_NS::Exception("ConcentrationIntegralOverArea::getVolume: Unsupported coordinate system");
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
