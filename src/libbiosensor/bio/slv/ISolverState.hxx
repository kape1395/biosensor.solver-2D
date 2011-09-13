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
#ifndef BIO_SLV_ISolverState_HXX
#define BIO_SLV_ISolverState_HXX
#include "../../biosensor.hxx"
#include "../dm/IDataModel.hxx"
BIO_SLV_NS_BEGIN


/**
 *  Solver state interface.
 */
class ISolverState
{
public:
    virtual ~ISolverState()
    {
        //  Empty virtual destructor.
    }

    /**
     *  Returns current (simulation) time.
     */
    virtual double getTime() = 0;

    /**
     *  Returns iteration number.
     */
    virtual long getIteration() = 0;

    /**
     *  Returns data model.
     */
    virtual BIO_DM_NS::IDataModel* getData() = 0;
};



BIO_SLV_NS_END
#endif
