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
#ifndef BIO_CFG_DefaultSymbolResolver_HXX
#define BIO_CFG_DefaultSymbolResolver_HXX
#include "../../biosensor.hxx"
#include "../Exception.hxx"
#include "ISymbolResolver.hxx"
#include <biosensor-xml.hxx>
#include <map>
#include <string>
BIO_CFG_NS_BEGIN


/**
 *  This is default implementation of the ISymbolResolver.
 *  It is configured by maps of type `string -> double' and also
 *  tries to resolve value from the symbol name, if symbol is not
 *  configured explicitly.
 */
class DefaultSymbolResolver : public BIO_CFG_NS::ISymbolResolver
{
private:
    std::map<std::string, double> symbols;

public:

    /**
     *  Default constructor. The resolver is created with no symbols
     *  configured.
     */
    DefaultSymbolResolver();

    /**
     *  A constructor for creating resolver with specified symbols
     *  configured.
     *  \param symbolDefs Symbol definitions.
     */
    DefaultSymbolResolver(std::map<std::string, double> symbolDefs);

    /**
     *  Destructor.
     */
    virtual ~DefaultSymbolResolver();

    /**
     *  \copydoc ISymbolResolver::getValueAsBool()
     */
    virtual bool getValueAsBool(std::string &symbolName);

    /**
     *  \copydoc ISymbolResolver::getValueAsLong()
     */
    virtual long getValueAsLong(std::string &symbolName);

    /**
     *  \copydoc ISymbolResolver::getValueAsDouble()
     */
    virtual double getValueAsDouble(std::string &symbolName);

    /**
     *  Configure additional symbols.
     *  \param symbolDefs Symbol definitions.
     *  \param override specifies, whether the definition of the symbol
     *      should be overridden, if particular symbol is already defined.
     */
    virtual void configure(std::map<std::string, double> symbolDefs, bool override = true);

protected:

    /**
     *  Converts string to double or throws an exception.
     *  \param string to be converted.
     *  \returns parsed double.
     *  \throws Exception if conversion is not possible.
     */
    double parseDouble(std::string &string);

};


BIO_CFG_NS_END
#endif
