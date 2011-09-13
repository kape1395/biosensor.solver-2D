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
#ifndef BIO_TRD_IntegratedConcentration_HXX
#define BIO_TRD_IntegratedConcentration_HXX
#include "../../biosensor.hxx"
#include "../cfg/StructureAnalyzer.hxx"
#include "../dm/IConcentrations.hxx"
#include "IIntegratedExpression.hxx"
#include <biosensor-xml.hxx>

BIO_TRD_NS_BEGIN


/**
 *  Expression used to integrate concentration of the particular substance.
 */
class IntegratedConcentration : public IIntegratedExpression
{
private:
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer;
    int substanceIndex;

public:

    /**
     *  Constructor.
     *
     *  \param structAnalyzer
     *  \param substanceName
     */
    IntegratedConcentration(
        BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
        BIO_XML_MODEL_NS::SubstanceName& substanceName
    );

    /**
     *  Destructor.
     */
    virtual ~IntegratedConcentration();

    /**
     *  Is this expression defined in the area with specified position.
     *
     *  \param h    Position in the hozirontal axis.
     *  \param v    Position in the vertical axis.
     *  \return     True id expression is defined.
     */
    virtual bool isDefined(int h, int v);

    /**
     *  Returns value of the expression in the concrete point.
     *  This operation will be called only for points, where expression is defined.
     *
     *  \param c    Concentration values.
     *  \return     Value of the expression.
     */
    virtual double getValue(BIO_DM_NS::IConcentrations* c);

};



BIO_TRD_NS_END
#endif
