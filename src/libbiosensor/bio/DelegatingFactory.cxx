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
#include "DelegatingFactory.hxx"

/* ************************************************************************** */
BIO_NS::DelegatingFactory::DelegatingFactory()
{
    //  Nothing to do here.
}


/* ************************************************************************** */
BIO_NS::DelegatingFactory::~DelegatingFactory()
{
    factories.clear();
    for (std::vector<IFactory*>::iterator ftd = factoriesToDelete.begin();
            ftd < factoriesToDelete.end(); ftd++)
    {
        delete *ftd;
    }
    factoriesToDelete.clear();
}


/* ************************************************************************** */
void BIO_NS::DelegatingFactory::addFactory(IFactory* factory, bool deleteAtDestruction)
{
    factories.push_back(factory);
    if (deleteAtDestruction)
        factoriesToDelete.push_back(factory);
}


/* ************************************************************************** */
BIO_SLV_NS::ISolver* BIO_NS::DelegatingFactory::createSolver(
    BIO_XML_MODEL_NS::Model* model
)
{
    for (std::vector<IFactory*>::iterator f = factories.begin(); f < factories.end(); f++)
    {
        BIO_SLV_NS::ISolver* solver = (*f)->createSolver(model);
        if (solver)
            return solver;
    }
    return 0;
}


/* ************************************************************************** */
BIO_SLV_NS::ISolverListener* BIO_NS::DelegatingFactory::createStopCondition(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::solver::StopCondition* stopCondition
)
{
    for (std::vector<IFactory*>::iterator f = factories.begin(); f < factories.end(); f++)
    {
        BIO_SLV_NS::ISolverListener* sl = (*f)->createStopCondition(solver, stopCondition);
        if (sl)
            return sl;
    }
    return 0;
}


/* ************************************************************************** */
BIO_SLV_NS::ISolverListener* BIO_NS::DelegatingFactory::createTimeStepAdjuster(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::solver::TimeStepAdjuster* timeStepAdjuster
)
{
    for (std::vector<IFactory*>::iterator f = factories.begin(); f < factories.end(); f++)
    {
        BIO_SLV_NS::ISolverListener* sl = (*f)->createTimeStepAdjuster(solver, timeStepAdjuster);
        if (sl)
            return sl;
    }
    return 0;
}


/* ************************************************************************** */
BIO_SLV_NS::ISolverListener* BIO_NS::DelegatingFactory::createOutput(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::SolverOutput* output
)
{
    for (std::vector<IFactory*>::iterator f = factories.begin(); f < factories.end(); f++)
    {
        BIO_SLV_NS::ISolverListener* sl = (*f)->createOutput(solver, output);
        if (sl)
            return sl;
    }
    return 0;
}


/* ************************************************************************** */
BIO_SLV_NS::ITransducer* BIO_NS::DelegatingFactory::createTransducer(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::Transducer* transducer
)
{
    for (std::vector<IFactory*>::iterator f = factories.begin(); f < factories.end(); f++)
    {
        BIO_SLV_NS::ITransducer* td = (*f)->createTransducer(solver, transducer);
        if (td)
            return td;
    }
    return 0;
}

/* ************************************************************************** */
