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
#ifndef BIO_CFG_ReactionAnalyzer_HXX
#define BIO_CFG_ReactionAnalyzer_HXX
#include "../../biosensor.hxx"
#include <biosensor-xml.hxx>
BIO_CFG_NS_BEGIN


/**
 *  Halper class to analyze reactions.
 */
class ReactionAnalyzer
{
public:
    static ReactionAnalyzer* newAnalyzer(BIO_XML_MODEL_NS::Reaction* reaction);

    virtual ~ReactionAnalyzer()
    {
        //  Nothing.
    }

    virtual bool isSubstrate(BIO_XML_MODEL_NS::SubstanceName& substance) = 0;
    virtual bool isProduct(BIO_XML_MODEL_NS::SubstanceName& substance) = 0;
};


BIO_CFG_NS_END
#endif
