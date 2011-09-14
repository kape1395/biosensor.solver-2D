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
#include "IntegralOverArea.hxx"
#include "../Logging.hxx"
#include "../Exception.hxx"
#include "../dm/IComposite2D.hxx"
#include "../dm/ConstantSegmentSplit.hxx"
#include "../dm/Cursor2DOpenBounds.hxx"
#include "../slv/IIterativeSolver.hxx"
#include <cmath>
#define LOGGER "libbiosensor::IntegralOverArea: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::IntegralOverArea::IntegralOverArea(
    BIO_SLV_NS::ISolver*            solver,
    BIO_XML_MODEL_NS::MediumName&   mediumName,
    IIntegratedExpression*          expression,
    BIO_CFG_NS::StructureAnalyzer*  structAnalyzer
) :
    CONST_PI(std::atan2(0, -1))
{
    BIO_DM_NS::IComposite2D* dataModel = 0;

    if (!(dataModel = dynamic_cast<BIO_DM_NS::IComposite2D*>(solver->getData())))
        throw Exception("IntegralOverArea: DataModel must implement IComposite2D.");

    this->structAnalyzer = structAnalyzer;
    this->expression = expression;


    for (int h = 0; h < dataModel->sizeH(); h++)
    {
        for (int v = 0; v < dataModel->sizeV(); v++)
        {
            if (structAnalyzer->getMediumName(h, v) && (structAnalyzer->getMediumName(h, v)->compare(mediumName) == 0))
            {
                if (!expression->isDefined(h, v))
                    throw Exception("IntegralOverArea: expression is not defined in the specified medium.");

                if (!dynamic_cast<BIO_DM_NS::ConstantSegmentSplit*>(dataModel->getArea(h, v)->getPointPositionsH()))
                    throw Exception("IntegralOverArea: only grid with constant steps is supported");

                if (!dynamic_cast<BIO_DM_NS::ConstantSegmentSplit*>(dataModel->getArea(h, v)->getPointPositionsV()))
                    throw Exception("IntegralOverArea: only grid with constant steps is supported");

                areas.push_back(dataModel->getArea(h, v));
            } // if name
        }
    }
    if (areas.size() == 0)
        throw Exception("IntegralOverArea: No areas were found with specified medium name.");

    this->open = false;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::IntegralOverArea::IntegralOverArea(
    BIO_SLV_NS::ISolver*            solver,
    IIntegratedExpression*          expression,
    BIO_CFG_NS::StructureAnalyzer*  structAnalyzer
) :
    CONST_PI(std::atan2(0, -1))
{
    BIO_DM_NS::IComposite2D* dataModel = 0;

    if (!(dataModel = dynamic_cast<BIO_DM_NS::IComposite2D*>(solver->getData())))
        throw Exception("IntegralOverArea: DataModel must implement IComposite2D.");

    this->structAnalyzer = structAnalyzer;
    this->expression = expression;


    for (int h = 0; h < dataModel->sizeH(); h++)
    {
        for (int v = 0; v < dataModel->sizeV(); v++)
        {
            if (expression->isDefined(h, v))
            {
                if (!dynamic_cast<BIO_DM_NS::ConstantSegmentSplit*>(dataModel->getArea(h, v)->getPointPositionsH()))
                    throw Exception("IntegralOverArea: only grid with constant steps is supported");

                if (!dynamic_cast<BIO_DM_NS::ConstantSegmentSplit*>(dataModel->getArea(h, v)->getPointPositionsV()))
                    throw Exception("IntegralOverArea: only grid with constant steps is supported");

                areas.push_back(dataModel->getArea(h, v));
            }
        }
    }
    if (areas.size() == 0)
        throw Exception("IntegralOverArea: No areas were found where provided expression is defined.");

    this->open = false;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::IntegralOverArea::~IntegralOverArea()
{
    areas.clear();
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::IntegralOverArea* BIO_TRD_NS::IntegralOverArea::forOpenArea(bool open)
{
    this->open = open;
    return this;
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_TRD_NS::IntegralOverArea::integrate()
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
double BIO_TRD_NS::IntegralOverArea::integrateSubArea(BIO_DM_NS::IGrid2D* area)
{
    LOG_TRACE(LOGGER << "integrateArea()...");

    BIO_DM_NS::ICursor2D* cursor = area->newGridCursor();
    if (open)
    {
        cursor = new BIO_DM_NS::Cursor2DOpenBounds(cursor, true);
    }

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
                   * expression->getValue(cursor->getConcentrations())
                   * stepH * stepV;

            LOG_TRACE(LOGGER << "integrateArea:"
                      << "\th=" << h
                      << "\tv=" << v
                      << "\tV=" << expression->getValue(cursor->getConcentrations())
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
double BIO_TRD_NS::IntegralOverArea::getVolume()
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
            throw BIO_NS::Exception("IntegralOverArea::getVolume: Unsupported coordinate system");
        }
    }

    LOG_TRACE(LOGGER << "getVolume()... Done, result=" << volume);
    return volume;
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_TRD_NS::IntegralOverArea::integrateOverVolume()
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
        throw BIO_NS::Exception("IntegralOverArea::getVolume: Unsupported coordinate system");
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
