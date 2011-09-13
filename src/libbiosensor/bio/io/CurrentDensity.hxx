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
#ifndef BIO_IO_CurrentDensity_HXX
#define BIO_IO_CurrentDensity_HXX
#include "../../biosensor.hxx"
#include "../slv/ISolver.hxx"
#include "IOutput.hxx"
#include "IContext.hxx"
#include <string>
#include <ostream>
BIO_IO_NS_BEGIN


/**
 *
 */
class CurrentDensity : public BIO_IO_NS::IOutput
{
private:

    std::string name;
    BIO_SLV_NS::ISolver* solver;
    BIO_IO_NS::IContext* Context;

    std::ostream* output;

public:
    /**
     *  Constructor.
     */
    CurrentDensity(
        std::string& name,
        BIO_SLV_NS::ISolver* solver,
        BIO_IO_NS::IContext* Context
    );

    /**
     *  Destructor.
     */
    virtual ~CurrentDensity();

    /**
     *  EventListener.
     */
    virtual void solveEventOccured();

    /**
     *  Reset listener's internal state.
     */
    virtual void reset()
    {
        //  Nothing to reset.
    }

};



BIO_IO_NS_END
#endif
