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
#ifndef BIO_CFG_IConfigValidator_HXX
#define BIO_CFG_IConfigValidator_HXX
#include "../../biosensor.hxx"
#include <biosensor-xml.hxx>
#include <vector>
#include <string>
BIO_CFG_NS_BEGIN


/**
 *  Implementations of this interface validates model by some criterias.
 */
class IConfigValidator
{
public:
    virtual ~IConfigValidator()
    {
        //  Empty virtual destructor.
    }

    virtual bool validate(BIO_XML_NS::model::Model* config) = 0;
    virtual std::vector<std::string>& getFailReasons() = 0;
};


BIO_CFG_NS_END
#endif
