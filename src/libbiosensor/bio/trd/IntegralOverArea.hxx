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
#ifndef BIO_TRD_IntegralOverArea_HXX
#define BIO_TRD_IntegralOverArea_HXX
#include "../../biosensor.hxx"
#include "../cfg/StructureAnalyzer.hxx"
#include "../slv/ISolver.hxx"
#include "../dm/IGrid2D.hxx"
#include "IIntegratedExpression.hxx"
#include <biosensor-xml.hxx>
#include <string>
#include <vector>
BIO_TRD_NS_BEGIN


/**
 *  Calculates an integral for specifled IntegratedExpression.
 *  By default the expression is integrated over a closed area.
 *  You can use #forOpenArea(bool) before calculating integral
 *  to get integral over an open area.
 */
class IntegralOverArea
{
private:
    const double CONST_PI;

    /**
     *  Used to work with area definitions.
     */
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer;

    /**
     *  Expression (function) to be integrated.
     */
    IIntegratedExpression* expression;

    /**
     *  Arreas to be integrated.
     */
    std::vector<BIO_DM_NS::IGrid2D*> areas;

    /**
     *  True, if each area should be considered open instead of closed.
     *  I.e. indicates, if boundaries should be used when calculating integral.
     *  If integral should be calculated over an open area, values on area boundaries
     *  are taken as equal to those one step from the boundary (like in non-leakage condition).
     */
    bool open;

public:

    /**
     *  Construct integrator by medium and substance name.
     */
    IntegralOverArea(
        BIO_SLV_NS::ISolver*            solver,
        BIO_XML_MODEL_NS::MediumName&   mediumName,
        IIntegratedExpression*          expression,
        BIO_CFG_NS::StructureAnalyzer*  structAnalyzer
    );

    /**
     *  Construct integrator for substance concentration over all biosensor.
     */
    IntegralOverArea(
        BIO_SLV_NS::ISolver*            solver,
        IIntegratedExpression*          expression,
        BIO_CFG_NS::StructureAnalyzer*  structAnalyzer
    );

    /**
     *  Destructor.
     */
    virtual ~IntegralOverArea();

    /**
     * Sets if integral should be calculated for open or closed areas.
     * \param open True, if integral should be over open area and false otherwise.
     * \return The same object.
     */
    IntegralOverArea* forOpenArea(bool open);
    
    /**
     *  Calculate integral of concentration over area.
     *
     *  \returns integral value.
     */
    virtual double integrate();

    /**
     *
     */
    virtual double integrateOverVolume();

    /**
     *  Returns area of the region, over which integration is performed.
     */
    virtual double getVolume();

    /**
     *  returns sub-areas, over which integrations is performed.
     */
    virtual const std::vector<BIO_DM_NS::IGrid2D*>& getIntegratedAreas()
    {
        return areas;
    }

    /**
     *  Returns integrated expression.
     */
    virtual IIntegratedExpression* getExpression()
    {
        return expression;
    }

private:

    /**
     *  Calculated integral for one sub-area.
     */
    double integrateSubArea(BIO_DM_NS::IGrid2D* area);

};



BIO_TRD_NS_END
#endif
