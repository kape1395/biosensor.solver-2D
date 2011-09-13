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
#ifndef BIO_TRD_IntegratedReaction_HXX
#define BIO_TRD_IntegratedReaction_HXX
#include "../../biosensor.hxx"
#include "../cfg/StructureAnalyzer.hxx"
#include "../dm/IConcentrations.hxx"
#include "IIntegratedExpression.hxx"
#include <biosensor-xml.hxx>

BIO_TRD_NS_BEGIN


/**
 *  Expression used to integrate "speed" of the particular reaction.
 */
class IntegratedReaction : public IIntegratedExpression
{
protected:
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer;
    BIO_XML_MODEL_NS::Reaction* reaction;

public:

    /**
     *  Constructor.
     *
     *  \param structAnalyzer
     */
    IntegratedReaction(
        BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
        BIO_XML_MODEL_NS::ReactionName& reactionName
    );

    /**
     *  Destructor.
     */
    virtual ~IntegratedReaction();

    /**
     *  Is this expression defined in the area with specified position.
     *
     *  \param h    Position in the hozirontal axis.
     *  \param v    Position in the vertical axis.
     *  \return     True id expression is defined.
     */
    virtual bool isDefined(int h, int v);

    /**
     *  Factory method for the various reactions.
     */
    static IntegratedReaction* newInstance(
        BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
        BIO_XML_MODEL_NS::ReactionName& reactionName
    );

};



BIO_TRD_NS_END
#endif
