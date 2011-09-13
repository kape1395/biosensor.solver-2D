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
#ifndef BIO_SLV_FD_IM2D_CornerSubSolver_HXX
#define BIO_SLV_FD_IM2D_CornerSubSolver_HXX
#include "../../../../biosensor-slv-fd.hxx"

BIO_SLV_FD_IM2D_NS_BEGIN
class CornerSubSolver;
BIO_SLV_FD_IM2D_NS_END

#include "../FiniteDifferencesSolverAnalyzer.hxx"
#include "BoundSubSolver.hxx"
#include <bio/cfg/StructureAnalyzer.hxx>
#include <bio/dm/ICursor1D.hxx>
#include <bio/dm/IConcentrations.hxx>
#include <vector>

BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *  Corners now are implemented by solving bounds for corner points too,
 *  when solving along the bound.
 */
class CornerSubSolver : public BIO_DM_NS::IConcentrations
{
private:
    std::vector< std::vector<BIO_DM_NS::ICursor1D*> > cursors;  //  cursors [globalSubstanceIndex][number]
    std::vector< BIO_DM_NS::ICursor1D* > cursorsPlain;          // just the same as cursors, but just a plain list.
    unsigned substCount;

public:

    /**
     *
     */
    CornerSubSolver(
        Solver* solver,
        int positionH,
        int positionV,
        BoundSubSolver* boundTop,
        BoundSubSolver* boundRight,
        BoundSubSolver* boundBottom,
        BoundSubSolver* boundLeft,
        BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
        BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer* fdAnalyzer
    );

    /**
     *
     */
    virtual ~CornerSubSolver();

    /**
     *  Solve...
     */
    void solveForward();

    /**
     *  Solve...
     */
    void solveBackward();

    /**
     *  return concentration of substance in this corner.
     *
     *  \param s    Global subsatnce index.
     */
    double getConcentration(int s);

    /**
     *  Set concentration...
     */
    void setConcentration(int s, double c);

protected:
    /**
     *  Apply initial conditions.
     */
    void applyInitialConditions(
        BoundSubSolver* boundTop,
        BoundSubSolver* boundRight,
        BoundSubSolver* boundBottom,
        BoundSubSolver* boundLeft
    );

};


BIO_SLV_FD_IM2D_NS_END

#endif
