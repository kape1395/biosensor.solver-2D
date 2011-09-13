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
#ifndef BIO_SLV_FD_EX2D_Solver_HXX
#define BIO_SLV_FD_EX2D_Solver_HXX
#include "../../../../biosensor-slv-fd.hxx"
#include <bio/slv/AbstractIterativeSolver.hxx>

BIO_SLV_FD_EX2D_NS_BEGIN


/**
 *  Solver implementing 2D explicit schema for the method of finite differences.
 */
class Solver : public BIO_SLV_NS::AbstractIterativeSolver
{
public:
    /**
     *  Consructor
     *
     *  @param config Model to be solved.
     */
    Solver(BIO_XML_NS::model::Model* config);

    /**
     *  Destructor..
     */
    virtual ~Solver();

    /**
     *  Returns data-model.
     */
    virtual BIO_DM_NS::IDataModel* getData();

    /**
     *  Returns transducer.
     */
    virtual ITransducer* getTransducer();

    /**
     *  Set state for the solver.
     */
    virtual void setState(BIO_SLV_NS::ISolverState* state);

    /**
     *  Get state for the solver.
     */
    virtual BIO_SLV_NS::ISolverState* getState();

protected:
    virtual void solveIteration();

};



BIO_SLV_FD_EX2D_NS_END

#endif
