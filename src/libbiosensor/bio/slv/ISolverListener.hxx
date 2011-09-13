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
#ifndef BIO_SLV_ISolverListener_HXX
#define BIO_SLV_ISolverListener_HXX
#include "../../biosensor.hxx"
#include "IIterativeSolver.hxx"
BIO_SLV_NS_BEGIN


/**
 *
 */
class ISolverListener
{
public:
    virtual ~ISolverListener()
    {
        //  Empty virtual destructor.
    }

    virtual void solveEventOccured() = 0;
    virtual void reset() = 0;
};



BIO_SLV_NS_END
#endif
