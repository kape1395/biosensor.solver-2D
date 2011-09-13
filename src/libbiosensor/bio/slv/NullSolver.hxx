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
#ifndef BIO_SLV_NullSolver_HXX
#define BIO_SLV_NullSolver_HXX
#include "../../biosensor.hxx"
#include "../dm/NullDM.hxx"
#include "AbstractSolver.hxx"
#include <biosensor-xml.hxx>
BIO_SLV_NS_BEGIN


/**
 *  Solver that doas nothing.
 */
class NullSolver : public AbstractSolver
{
private:
    BIO_DM_NS::NullDM *data;

public:

    /**
     *  Constructor.
     */
    NullSolver(BIO_XML_NS::model::Model* config);

    /**
     *  Destructor.
     */
    virtual ~NullSolver();

    /**
     *  Returns data-model.
     */
    virtual BIO_DM_NS::IDataModel* getData();

    /**
     *  Solves nothing.
     */
    virtual void solve();

};


BIO_SLV_NS_END
#endif
