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
#ifndef BIO_SLV_FD_IM1D_AreaSubSolver_HXX
#define BIO_SLV_FD_IM1D_AreaSubSolver_HXX
#include "../../../../biosensor-slv-fd.hxx"

BIO_SLV_FD_IM1D_NS_BEGIN
class AreaSubSolver;
BIO_SLV_FD_IM1D_NS_END

#include "Solver.hxx"
#include "../im2d/Solver.hxx"
#include "../im2d/AreaSubSolver.hxx"

BIO_SLV_FD_IM1D_NS_BEGIN


/**
 *  This solver is responsible for one homogenous rectangular area in 1D space.
 *  Only data model is changed comparing to the 2D implementation.
 *
 *  TODO: TimeStep for me is not clear... Why there is no /2 in 2D?
 */
class AreaSubSolver : public BIO_SLV_FD_IM2D_NS::AreaSubSolver
{

public:

    /**
     *  Constructor.
     */
    AreaSubSolver(
        BIO_SLV_FD_IM2D_NS::Solver* solver,
        int positionH,
        int positionV,
        BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
        BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer* fdAnalyzer
    );

    /**
     *  Destructor.
     */
    virtual ~AreaSubSolver();

    virtual void solveVerticalForward();

    virtual void solveVerticalBackward();


protected:

    virtual double**** createData();
    virtual void deleteData(double**** data);


};



BIO_SLV_FD_IM1D_NS_END

#endif
