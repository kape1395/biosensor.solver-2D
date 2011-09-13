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
#ifndef BIO_SLV_ISolver_HXX
#define BIO_SLV_ISolver_HXX
#include "../../biosensor.hxx"
#include "../dm/IDataModel.hxx"
#include "ISolverState.hxx"
#include "ITransducer.hxx"
#include <biosensor-xml.hxx>
BIO_SLV_NS_BEGIN


/**
 *  Solver performs a simulation of the specified biosensor.
 */
class ISolver
{
public:
    virtual ~ISolver()
    {
        //  Empty virtual destructor.
    }

    /**
     *  Simulate an elxecution of the specified biosensor. Biosensor model
     *  is specified during the construction of the solver..
     */
    virtual void solve() = 0;

    /**
     *  Returns current concentrations of the substances.
     */
    virtual BIO_DM_NS::IDataModel* getData() = 0;

    /**
     *  Returns configuration (model), that was used for the simulation.
     */
    virtual BIO_XML_NS::model::Model* getConfig() = 0;

    /**
     *  Returns transducer, used in the simulation. Transtuder generates an
     *  output of the biosensor.
     */
    virtual ITransducer* getTransducer() = 0;

    /**
     *  Tels, if steady state is alredy reached by the solver.
     */
    virtual bool isSteadyStateReached() = 0;

    /**
     *  Set state for the solver.
     */
    virtual void setState(ISolverState* state) = 0;

    /**
     *  Get state for the solver.
     */
    virtual ISolverState* getState() = 0;

};



BIO_SLV_NS_END
#endif
