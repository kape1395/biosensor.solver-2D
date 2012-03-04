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
#include "DefaultSymbolResolver.hxx"
#include <algorithm>
#include <cmath>
#include <sstream>
#include "../Logging.hxx"
#define LOGGER "libbiosensor::DefaultSymbolResolver: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_CFG_NS::DefaultSymbolResolver::DefaultSymbolResolver()
{
    symbols.clear();
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_CFG_NS::DefaultSymbolResolver::DefaultSymbolResolver(
    std::map<std::string, double> symbolDefs)
{
    symbols = symbolDefs;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_CFG_NS::DefaultSymbolResolver::~DefaultSymbolResolver()
{
    symbols.clear();
}


/* ************************************************************************** */
/* ************************************************************************** */
bool BIO_CFG_NS::DefaultSymbolResolver::getValueAsBool(std::string &symbolName)
{
    if (symbols.find(symbolName) == symbols.end())
    {
        std::string name = symbolName;
        std::transform(name.begin(), name.end(), name.begin(), ::tolower);
        double value;
        if (name.compare("true") == 0 || name.compare("y") == 0 || name.compare("1") == 0)
        {
            value = 1.0;
        }
        else if (name.compare("false") == 0 || name.compare("n") == 0 || name.compare("0") == 0)
        {
            value = 0.0;
        }
        else
        {
            LOG_ERROR("Unable to resolve symbol to a boolean: name=" << symbolName);
            throw Exception("Unable to resolve symbol to a boolean value");
        }
        LOG_INFO(LOGGER
                 << "Implicit symbol declaration (bool): "
                 << symbolName << "->" << value);
        symbols.insert(std::pair<std::string, double>(symbolName, value));
    }

    double value = symbols[symbolName];
    bool valueBool = value > ZERO_MAX;
    LOG_DEBUG(LOGGER << "Resolved symbol: " << symbolName << "=" << value << " => " << valueBool << " (bool)");
    return valueBool;
}


/* ************************************************************************** */
/* ************************************************************************** */
long BIO_CFG_NS::DefaultSymbolResolver::getValueAsLong(std::string &symbolName)
{
    if (symbols.find(symbolName) == symbols.end())
    {
        double value = parseDouble(symbolName);
        symbols.insert(std::pair<std::string, double>(symbolName, value));
        LOG_INFO(LOGGER
                 << "Implicit symbol declaration (long): "
                 << symbolName << "->" << value);
    }

    double value = symbols[symbolName];
    long valueLong = ::lround(symbols[symbolName]);
    LOG_DEBUG(LOGGER << "Resolved symbol: " << symbolName << "=" << value << " => " << valueLong << " (long)");
    return valueLong;
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_CFG_NS::DefaultSymbolResolver::getValueAsDouble(std::string &symbolName)
{
    if (symbols.find(symbolName) == symbols.end())
    {
        double value = parseDouble(symbolName);
        symbols.insert(std::pair<std::string, double>(symbolName, value));
        LOG_INFO(LOGGER
                 << "Implicit symbol declaration (double): "
                 << symbolName << "->" << value);
    }

    double value = symbols[symbolName];
    LOG_DEBUG(LOGGER << "Resolved symbol: " << symbolName << "=" << value << " (double)");
    return value;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_CFG_NS::DefaultSymbolResolver::configure(
    std::map<std::string, double> symbolDefs,
    bool override)
{
    std::map<std::string, double>::iterator it;
    for (it = symbolDefs.begin(); it != symbolDefs.end(); it++)
    {
        std::string name = it->first;
        double value = it->second;
        if (symbols.find(name) == symbols.end())
        {
            symbols.insert(*it);
        }
        else if (override)
        {
            symbols[name] = value;
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_CFG_NS::DefaultSymbolResolver::parseDouble(std::string &string)
{
    std::istringstream stream(string);
    double value;
    stream >> value;

    if (stream.bad() || stream.fail())
    {
        LOG_ERROR("Unable to resolve symbol to a double:"
                  << " name=" << string
                  << " parsedValue=" << value);
        throw Exception("Unable to resolve symbol to a double value");
    }

    return value;
}


/* ************************************************************************** */
