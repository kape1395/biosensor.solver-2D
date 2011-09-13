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
#include "SubSolverFactory.hxx"
#include "AreaSubSolver.hxx"

BIO_SLV_FD_IM2D_NS::AreaSubSolver*
BIO_SLV_FD_IM1D_NS::SubSolverFactory::createAreaSubSolver(
    BIO_SLV_FD_IM2D_NS::Solver* solver,
    int h, int v
)
{
    AreaSubSolver* ass = new BIO_SLV_FD_IM1D_NS::AreaSubSolver(
        solver, h, v,
        solver->getStructAnalyzer(),
        solver->getFDAnalyzer()
    );
    ass->initialize();
    return ass; // ;)
}
