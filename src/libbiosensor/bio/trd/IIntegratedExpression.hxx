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
#ifndef BIO_TRD_IIntegratedExpression_HXX
#define BIO_TRD_IIntegratedExpression_HXX
#include "../../biosensor.hxx"
#include "../dm/IConcentrations.hxx"

BIO_TRD_NS_BEGIN


/**
 *  Expression which can be integrated (over an area).
 *  Implementations of this interface are used in the class IntegralOverArea.
 */
class IIntegratedExpression
{
public:

    /**
     *  Destructor.
     */
    virtual ~IIntegratedExpression() {}

    /**
     *  Is this expression defined in the area with specified position.
     *
     *  \param h    Position in the hozirontal axis.
     *  \param v    Position in the vertical axis.
     *  \return     True id expression is defined.
     */
    virtual bool isDefined(int h, int v) = 0;

    /**
     *  Returns value of the expression in the concrete point.
     *  This operation will be called only for points, where expression is defined.
     *
     *  \param c    Concentration values.
     *  \return     Value of the expression.
     */
    virtual double getValue(BIO_DM_NS::IConcentrations* c) = 0;

};



BIO_TRD_NS_END
#endif
