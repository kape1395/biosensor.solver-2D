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
#ifndef BIO_SLV_FD_IM2D_ISubSolverFactory_HXX
#define BIO_SLV_FD_IM2D_ISubSolverFactory_HXX
#include "../../../../biosensor-slv-fd.hxx"

BIO_SLV_FD_IM2D_NS_BEGIN
class ISubSolverFactory;
BIO_SLV_FD_IM2D_NS_END

#include "Solver.hxx"
#include "AreaSubSolver.hxx"

BIO_SLV_FD_IM2D_NS_BEGIN


class ISubSolverFactory
{
public:

    virtual ~ISubSolverFactory()
    {
        //  nothing.
    }

    virtual AreaSubSolver* createAreaSubSolver(
        Solver* solver,
        int h, int v
    ) = 0;

};

BIO_SLV_FD_IM2D_NS_END

#endif
