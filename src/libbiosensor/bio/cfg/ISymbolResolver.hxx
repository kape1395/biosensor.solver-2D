/*
 * Copyright 2012 Karolis Petrauskas
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
#ifndef BIO_CFG_ISymbolResolver_HXX
#define BIO_CFG_ISymbolResolver_HXX
#include "../../biosensor.hxx"
#include "../Exception.hxx"
#include <biosensor-xml.hxx>
#include <vector>
#include <string>
BIO_CFG_NS_BEGIN


/**
 *  This interface should be used to get symbol values
 *  instead of direct access.
 */
class ISymbolResolver
{
public:

    /**
     * Empty virtual destructor.
     */
    virtual ~ISymbolResolver()
    {
        //  Nothing.
    }

    /**
     *  Returns symbol value as a boolean.
     *  \param symbolName Symbol name.
     *  \returns value.
     *  \throws Exception if resolution is not possible.
     */
    virtual bool getValueAsBool(std::string &symbolName) = 0;

    /**
     *  Returns symbol value as a integral number.
     *  \param symbolName Symbol name.
     *  \returns value.
     *  \throws Exception if resolution is not possible.
     */
    virtual long getValueAsLong(std::string &symbolName) = 0;

    /**
     *  Returns symbol value as a double precision float.
     *  \param symbolName Symbol name.
     *  \returns value.
     *  \throws Exception if resolution is not possible.
     */
    virtual double getValueAsDouble(std::string &symbolName) = 0;

};


BIO_CFG_NS_END
#endif
