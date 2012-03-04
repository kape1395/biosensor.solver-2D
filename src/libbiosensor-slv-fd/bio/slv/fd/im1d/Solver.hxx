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
#ifndef BIO_SLV_FD_IM1D_Solver_HXX
#define BIO_SLV_FD_IM1D_Solver_HXX
#include "../../../../biosensor-slv-fd.hxx"

BIO_SLV_FD_IM1D_NS_BEGIN
class Solver;
BIO_SLV_FD_IM1D_NS_END

#include "../im2d/ISubSolverFactory.hxx"
#include "../im2d/Solver.hxx"
#include "../im2d/AreaSubSolver.hxx"
#include "../FiniteDifferencesSolverAnalyzer.hxx"

BIO_SLV_FD_IM1D_NS_BEGIN


/**
 *  Solver: one-dimensional, implemented using implicit scheme.
 */
class Solver : public BIO_SLV_FD_IM2D_NS::Solver
{

public:
    /**
     *  Constructor.
     */
    Solver(
        BIO_XML_NS::model::Model*                       config,
        BIO_NS::IFactory*                               factory,
        BIO_SLV_FD_IM2D_NS::ISubSolverFactory*          subSolverFactory,
        BIO_CFG_NS::StructureAnalyzer*                  structAnalyzer,
        BIO_CFG_NS::BoundAnalyzer*                      boundAnalyzer,
        BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer* fdAnalyzer,
        BIO_CFG_NS::ISymbolResolver*                    symbolResolver
    );

    /**
     *  Destructor.
     */
    virtual ~Solver();

    /**
     *  Overrides im2d::Solver::solveIteration().
     *  This method solves one full iteration (step in time).
     *  This implementation solver vertical direction only, as in this software
     *  in the case of 1d model the axis is treated as a vertical one.
     */
    virtual void solveIteration();

};



BIO_SLV_FD_IM1D_NS_END

#endif
