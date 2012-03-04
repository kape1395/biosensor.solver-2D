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
#include "Factory.hxx"
#include <bio/Logging.hxx>
#include <bio/Exception.hxx>
#include "ex2d/Solver.hxx"
#include "im1d/Solver.hxx"
#include "im1d/SubSolverFactory.hxx"
#include "im2d/Solver.hxx"
#include "im2d/SubSolverFactory.hxx"
#define LOGGER "libbiosensor-slv-fd: "

/* ************************************************************************** */
BIO_SLV_FD_NS::Factory::Factory(
    IFactory* rootFactory,
    BIO_CFG_NS::BoundAnalyzer* boundAnalyzer,
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
    BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer *fdAnalyzer,
    BIO_CFG_NS::ISymbolResolver* symbolResolver
)
{
    this->rootFactory = rootFactory;
    this->boundAnalyzer = boundAnalyzer;
    this->structAnalyzer = structAnalyzer;
    this->fdAnalyzer = fdAnalyzer;
    this->symbolResolver = symbolResolver;
}


/* ************************************************************************** */
BIO_SLV_FD_NS::Factory::~Factory()
{
    //  Nothing to do here.
}


/* ************************************************************************** */
BIO_SLV_NS::ISolver* BIO_SLV_FD_NS::Factory::createSolver(
    BIO_XML_MODEL_NS::Model* model
)
{
    if (model == 0)
        throw BIO_NS::Exception("Model is NULL in configuration, no solver can be created.");


    if (dynamic_cast<BIO_XML_NS::model::solver::Explicit1D*>(&model->solver()) != 0)
    {
        LOG_INFO(LOGGER << "Selected solver: bio::xml::model::solver::Explicit1D -> ???");
        throw BIO_NS::Exception("Not implemented: bio::xml::model::solver::Explicit1D");
    }
    else if (dynamic_cast<BIO_XML_NS::model::solver::Implicit1D*>(&model->solver()) != 0)
    {
        LOG_INFO(LOGGER << "Selected solver: bio::xml::model::solver::Implicit1D -> BIO_SLV_FD_NS::im1d::Solver");
        BIO_SLV_FD_IM1D_NS::SubSolverFactory subSolverFactory;
        BIO_SLV_FD_NS::im1d::Solver* solver = new BIO_SLV_FD_NS::im1d::Solver(
            model,
            rootFactory,
            &subSolverFactory,
            structAnalyzer,
            boundAnalyzer,
            fdAnalyzer,
            symbolResolver
        );

        return solver;
    }
    else if (dynamic_cast<BIO_XML_NS::model::solver::Explicit2D*>(&model->solver()) != 0)
    {
        LOG_INFO(LOGGER << "Selected solver: bio::xml::model::solver::Explicit2D -> BIO_SLV_FD_NS::ex2d::Solver");
        BIO_SLV_FD_NS::ex2d::Solver* solver = new BIO_SLV_FD_NS::ex2d::Solver(model);

        return solver;
    }
    else if (dynamic_cast<BIO_XML_NS::model::solver::Implicit2D*>(&model->solver()) != 0)
    {
        LOG_INFO(LOGGER << "Selected solver: bio::xml::model::solver::Implicit2D -> BIO_SLV_FD_NS::im2d::Solver");
        BIO_SLV_FD_IM2D_NS::SubSolverFactory subSolverFactory;
        BIO_SLV_FD_IM2D_NS::Solver* solver = new BIO_SLV_FD_NS::im2d::Solver(
            model,
            rootFactory,
            &subSolverFactory,
            structAnalyzer,
            boundAnalyzer,
            fdAnalyzer,
            symbolResolver
        );

        return solver;
    }
    else
    {
        LOG_DEBUG(LOGGER << "I don't know the requested solver, so returning 0.");
        return 0;
    }
}


/* ************************************************************************** */
BIO_SLV_NS::ISolverListener* BIO_SLV_FD_NS::Factory::createStopCondition(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::solver::StopCondition* stopCondition
)
{
    //  This factory supplies no new stop conditions.
    return 0;
}


/* ************************************************************************** */
BIO_SLV_NS::ISolverListener* BIO_SLV_FD_NS::Factory::createTimeStepAdjuster(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::solver::TimeStepAdjuster* timeStepAdjuster
)
{
    //  This factory is supplying no time step adjusters.
    return 0;
}


/* ************************************************************************** */
BIO_SLV_NS::ISolverListener* BIO_SLV_FD_NS::Factory::createOutput(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::SolverOutput* output
)
{
    //  This factory supplies no new output generators.
    return 0;
}


/* ************************************************************************** */
BIO_SLV_NS::ITransducer* BIO_SLV_FD_NS::Factory::createTransducer(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::Transducer* transducer
)
{
    //  This factory supplies no new transducers.
    return 0;
}

/* ************************************************************************** */
