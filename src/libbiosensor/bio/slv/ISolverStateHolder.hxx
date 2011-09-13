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
#ifndef BIO_SLV_ISolverStateHolder_HXX
#define BIO_SLV_ISolverStateHolder_HXX
#include "../../biosensor.hxx"
#include "ISolverState.hxx"
BIO_SLV_NS_BEGIN


/**
 *  Something that can hold/store solver's state.
 */
class ISolverStateHolder
{
public:
    virtual ~ISolverStateHolder()
    {
        //  Empty virtual destructor.
    }

    /**
     *  Returns solver state.
     */
    virtual BIO_SLV_NS::ISolverState* getSolverState() = 0;

    /**
     *  Remembers state of the solver.
     */
    virtual void setSolverState(BIO_SLV_NS::ISolverState* state) = 0;

    /**
     *  Tells, if this holder has a state,
     */
    virtual bool hasSolverState() = 0;

};



BIO_SLV_NS_END
#endif
