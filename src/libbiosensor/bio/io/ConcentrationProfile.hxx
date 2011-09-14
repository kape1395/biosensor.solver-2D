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
#ifndef BIO_IO_ConcentrationProfile_HXX
#define BIO_IO_ConcentrationProfile_HXX
#include "../../biosensor.hxx"
#include "../dm/IGrid2D.hxx"
#include "../dm/ICursor2D.hxx"
#include "../slv/ISolverStateHolder.hxx"
#include "IOutput.hxx"
#include "IContext.hxx"
#include "IRepeatable.hxx"
#include "ConcentrationProfileReader.hxx"
#include <string>
BIO_IO_NS_BEGIN


/**
 *
 */
class ConcentrationProfile :
    public BIO_IO_NS::IOutput,
    public BIO_IO_NS::IRepeatable,
    public BIO_SLV_NS::ISolverStateHolder
{
private:

    std::string name;
    BIO_SLV_NS::ISolver* solver;
    BIO_IO_NS::IContext* context;


    bool indexed;
    bool haveLastOutput;
    bool overwrite;
    long currentIndex;

    /**
     *  Precision, used when formatting numbers for the output.
     *  Default precision is used is -1 is specified.
     */
    int precision;

    BIO_IO_NS::ConcentrationProfileReader* lastStateReader;

public:
    /**
     *  Constructor.
     */
    ConcentrationProfile(
        std::string& name,
        BIO_SLV_NS::ISolver* solver,
        BIO_IO_NS::IContext* context
    );

    /**
     *  Destructor.
     */
    virtual ~ConcentrationProfile();

    /* ********************************************************************** */
    /**
     *  EventListener.
     */
    virtual void solveEventOccured();

    /**
     *  Reset listener's internal state.
     */
    virtual void reset();

    /* ********************************************************************** */
    /**
     *  Set true, if this output writer will be called multiple times.
     */
    virtual void setRepeatable(bool repeatable);

    /* ********************************************************************** */
    /**
     *  Set if output overwriting is allowed.
     */
    virtual void setOverwrite(bool overwrite);

    /**
     *  Precision, used when formatting numbers for the output.
     */
    virtual void setPrecision(int precision);

    /* ********************************************************************** */
    /**
     *  Returns solver state.
     *  This is implementation of ISolverStateListener.
     */
    virtual BIO_SLV_NS::ISolverState* getSolverState();

    /**
     *  Remembers state of the solver.
     *  This is implementation of ISolverStateListener.
     */
    virtual void setSolverState(BIO_SLV_NS::ISolverState* state);

    /**
     *  Tells, if this holder has a state,
     *  This is implementation of ISolverStateListener.
     */
    virtual bool hasSolverState();

    /* ********************************************************************** */
protected:

    /**
     *  Remembers state of the solver.
     *  This is implementation of ISolverStateListener.
     */
    virtual void setSolverState(BIO_SLV_NS::ISolverState* state, bool overwrite);

    /**
     *  Returns reader, that is configured to read last concentration file,
     *  produced by this writer.
     *  Returned object should be deleted by the caller.
     */
    virtual BIO_IO_NS::ConcentrationProfileReader* createReaderForLastOutput();

};



BIO_IO_NS_END
#endif
