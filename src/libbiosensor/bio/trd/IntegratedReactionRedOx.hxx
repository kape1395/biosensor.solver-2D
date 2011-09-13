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
#ifndef BIO_TRD_IntegratedReactionRedOx_HXX
#define BIO_TRD_IntegratedReaction_HXX
#include "../../biosensor.hxx"
#include "../dm/IConcentrations.hxx"
#include "IntegratedReaction.hxx"
#include <biosensor-xml.hxx>

BIO_TRD_NS_BEGIN


/**
 *  Expression used to integrate "speed" of the particular RedOx reaction.
 */
class IntegratedReactionRedOx : public IntegratedReaction
{
private:
    std::vector<int> substrates;
    double rate;

public:

    /**
     *  Constructor.
     *
     *  \param structAnalyzer
     */
    IntegratedReactionRedOx(
        BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
        BIO_XML_MODEL_NS::ReactionName& reactionName
    );

    /**
     *  Destructor.
     */
    virtual ~IntegratedReactionRedOx();

    virtual double getValue(BIO_DM_NS::IConcentrations* c);

};



BIO_TRD_NS_END
#endif
