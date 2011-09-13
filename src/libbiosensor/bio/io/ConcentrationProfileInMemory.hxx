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
#ifndef BIO_IO_ConcentrationProfileInMemory_HXX
#define BIO_IO_ConcentrationProfileInMemory_HXX
#include "../../biosensor.hxx"
#include "../dm/IDataModel.hxx"
#include "../dm/IGrid2D.hxx"
#include "../dm/ICursor2D.hxx"
#include "../dm/ISegmentSplit.hxx"
#include "../dm/IConcentrations.hxx"
#include "../dm/AbstractCursor2D.hxx"
#include "../cfg/StructureAnalyzer.hxx"
#include "../slv/ISolverState.hxx"
#include "../slv/ISolverStateHolder.hxx"
#include "../slv/ISolverListener.hxx"
#include "../Exception.hxx"
#include <boost/filesystem/path.hpp>
#include <boost/filesystem.hpp>
#include <string>
BIO_IO_NS_BEGIN


/**
 */
class ConcentrationProfileInMemory :
        public BIO_SLV_NS::ISolverStateHolder,
        public BIO_SLV_NS::ISolverListener,
        public BIO_SLV_NS::ISolverState,
        public BIO_DM_NS::IDataModel,
        public BIO_DM_NS::IGrid2D
{
protected:
    class Cursor2DImpl;

private:
    BIO_SLV_NS::IIterativeSolver* iterativeSolver;
    BIO_SLV_NS::ISolver* solver;
    BIO_DM_NS::IGrid2D* solverData;

    bool hasState;
    long iterationNumber;
    double solvedTime;

    int sizeS;          ///< depth of the matrix (substanceCount).
    int sizeH;          ///< size of the matrix
    int sizeV;          ///< size of the matrix
    double ***matrix;   ///< rawData layed out in the matrix form [h][v][s].

public:
    /**
     *  Constructor.
     */
    ConcentrationProfileInMemory(
        BIO_SLV_NS::ISolver* solver
    );

    /**
     *  Destructor.
     */
    virtual ~ConcentrationProfileInMemory();


    /* ********************************************************************** */
    /**
     *  Returns solver state.
     *  Implementation of ISolverStateHolder;
     */
    virtual BIO_SLV_NS::ISolverState* getSolverState();

    /**
     *  Remembers state of the solver.
     *  Implementation of ISolverStateHolder;
     */
    virtual void setSolverState(BIO_SLV_NS::ISolverState* state);

    /**
     *  Tells, if this holder has a state,
     *  Implementation of ISolverStateHolder;
     */
    virtual bool hasSolverState();

    /* ********************************************************************** */
    /**
     *  Implementation of ISolverListener.
     */
    virtual void solveEventOccured();

    /**
     *  Implementation of ISolverListener.
     */
    virtual void reset();


    /* ********************************************************************** */
    /**
     *  Returns solvedTime.
     *  This is implementation of ISolverState.
     */
    virtual double getTime();

    /**
     *  Returns iteration number.
     *  This is implementation of ISolverState.
     */
    virtual long getIteration();

    /**
     *  Returns concentrations.
     *  This is implementation of ISolverState.
     */
    virtual BIO_DM_NS::IDataModel* getData();

    /* ********************************************************************** */
    /**
     *  Copies the state from source to this data model.
     *  This is implementation of IDataModel.
     */
    virtual void setState(BIO_DM_NS::IDataModel *source);

    /* ********************************************************************** */
    /**
     *  Implementation of IGrid2D
     */
    virtual BIO_DM_NS::ISegmentSplit* getPointPositionsH();

    /**
     *  Implementation of IGrid2D
     */
    virtual BIO_DM_NS::ISegmentSplit* getPointPositionsV();

    /**
     *  Implementation of IGrid2D
     */
    virtual BIO_DM_NS::ICursor2D* newGridCursor();

    /**
     *  Implementation of IGrid
     */
    virtual int getSubstanceCount();
    /**
     *  Implementation of IGrid
     */
    virtual BIO_XML_NS::model::Substance* getSubstanceConf(int index);

protected:

    /* ********************************************************************** */
    /**
     *  Cursor impl.
     */
    class Cursor2DImpl : public BIO_DM_NS::AbstractCursor2D, public BIO_DM_NS::IConcentrations
    {
    private:
        ConcentrationProfileInMemory* source;

    public:
        Cursor2DImpl(ConcentrationProfileInMemory* source);
        virtual ~Cursor2DImpl();
        virtual BIO_DM_NS::IConcentrations *getConcentrations();
        virtual double getConcentration(int substanceNr);
        virtual void setConcentration(int substanceNr, double concentration);
    };

};



BIO_IO_NS_END
#endif
