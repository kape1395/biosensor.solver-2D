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
#ifndef BIO_DelegatingFactory_HXX
#define BIO_DelegatingFactory_HXX
#include "../biosensor.hxx"
#include "IFactory.hxx"
#include "slv/ISolver.hxx"
#include "slv/ISolverListener.hxx"
#include <biosensor-xml.hxx>
#include <vector>

BIO_NS_BEGIN


/**
 */
class DelegatingFactory : public IFactory
{
protected:
    std::vector<IFactory*> factories;
    std::vector<IFactory*> factoriesToDelete;


public:

    /**
     *
     */
    DelegatingFactory();

    /**
     *
     */
    virtual ~DelegatingFactory();

    /**
     *
     */
    void addFactory(IFactory* factory, bool deleteAtDestruction);

    /**
     *  Create a solver from a model.
     *
     *  \returns
     *      Created solver or 0 - if does not know the solver,
     *      specified in the model.
     */
    virtual BIO_SLV_NS::ISolver* createSolver(
        BIO_XML_MODEL_NS::Model* model
    );

    /**
     *
     */
    virtual BIO_SLV_NS::ISolverListener* createStopCondition(
        BIO_SLV_NS::ISolver* solver,
        BIO_XML_MODEL_NS::solver::StopCondition* stopCondition
    );

    /**
     *  Create time step adjuster by specification.
     */
    virtual BIO_SLV_NS::ISolverListener* createTimeStepAdjuster(
        BIO_SLV_NS::ISolver* solver,
        BIO_XML_MODEL_NS::solver::TimeStepAdjuster* timeStepAdjuster
    );

    /**
     *
     */
    virtual BIO_SLV_NS::ISolverListener* createOutput(
        BIO_SLV_NS::ISolver* solver,
        BIO_XML_MODEL_NS::SolverOutput* output
    );

    /**
     *
     */
    virtual BIO_SLV_NS::ITransducer* createTransducer(
        BIO_SLV_NS::ISolver* solver,
        BIO_XML_MODEL_NS::Transducer* transducer
    );

};

BIO_NS_END

#endif
