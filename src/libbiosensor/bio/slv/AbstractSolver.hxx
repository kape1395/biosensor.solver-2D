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
#ifndef BIO_SLV_ABSTRACTSOLVER_HXX
#define BIO_SLV_ABSTRACTSOLVER_HXX
#include "../../biosensor.hxx"
#include "ISolver.hxx"
#include "../dm/IDataModel.hxx"
#include <biosensor-xml.hxx>
BIO_SLV_NS_BEGIN


/**
 *  Abstract implementation of ISolver. It is very simple now.
 */
class AbstractSolver : public ISolver
{
private:
    BIO_XML_NS::model::Model* config;

public:

    /**
     *  Constructor.
     */
    AbstractSolver(BIO_XML_NS::model::Model* config);

    /**
     *  Destructor.
     */
    virtual ~AbstractSolver();

    /**
     *  Returns configuration, supplied for this solver.
     */
    virtual BIO_XML_NS::model::Model* getConfig();

};


BIO_SLV_NS_END
#endif
