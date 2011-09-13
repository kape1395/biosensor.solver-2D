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
#ifndef BIO_SLV_FD_IM2D_DataModel_HXX
#define BIO_SLV_FD_IM2D_DataModel_HXX
#include "../../../../biosensor-slv-fd.hxx"

BIO_SLV_FD_IM2D_NS_BEGIN
class DataModel;
BIO_SLV_FD_IM2D_NS_END

#include <bio/dm/IDataModel.hxx>
#include <bio/dm/IGrid2D.hxx>
#include <bio/dm/ICursor2D.hxx>
#include <bio/dm/IConcentrations.hxx>
#include <bio/dm/IComposite2D.hxx>
#include <bio/dm/ISegmentSplit.hxx>
#include <bio/dm/AbstractCursor2D.hxx>
#include "Solver.hxx"

BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *  DataModel facade for users of Solver class.
 */
class DataModel :
        public BIO_DM_NS::IDataModel,
        public BIO_DM_NS::IGrid2D,
        public BIO_DM_NS::IComposite2D
{
private:
    Solver* solver;
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer;
    unsigned *areaRangesH;   // Point number, at which starts h^th area.
    unsigned *areaRangesV;   // Point number, at which starts v^th area.
    unsigned areaCountH;
    unsigned areaCountV;
    unsigned pointCountH;    // Total count of points in horizontal axis.
    unsigned pointCountV;    // Total count of points in vertical axis.
    BIO_DM_NS::ISegmentSplit* segmentSplitH;
    BIO_DM_NS::ISegmentSplit* segmentSplitV;

public:

    /**
     *  Constructor.
     */
    DataModel(
        Solver* solver,
        BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
        BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer* fdAnalyzer
    );

    /**
     *  Destructor.
     */
    virtual ~DataModel();

    /**
     *  Copies the state from source to this data model.
     */
    virtual void setState(BIO_DM_NS::IDataModel *source);

    /**
     *
     */
    virtual int getSubstanceCount();

    /**
     *
     */
    virtual BIO_XML_NS::model::Substance* getSubstanceConf(int index);

    /**
     *
     */
    virtual BIO_DM_NS::ISegmentSplit* getPointPositionsH();

    /**
     *
     */
    virtual BIO_DM_NS::ISegmentSplit* getPointPositionsV();

    /**
     *  Returns cursor, that can be used to iterate over solvers modelled area.
     *  NOTE: Cursor, returned by this method must be deleted by the caller.
     *
     *  \return new cursor.
     */
    virtual BIO_DM_NS::ICursor2D* newGridCursor();


private:

    /**
     *  Cursor...
     */
    class Cursor : public BIO_DM_NS::AbstractCursor2D, public BIO_DM_NS::IConcentrations
    {
    private:
        DataModel* dataModel;
        unsigned currentAreaH;
        unsigned currentAreaV;
        bool currentOnBoundH;
        bool currentOnBoundV;

    public:

        /**
         *  Constructor.
         */
        Cursor(DataModel* dataModel);

        /**
         *  Destructor.
         */
        virtual ~Cursor();

        /**
         *  Returns substance concentrations at the current position.
         *  This is implementation of IGrid2D.
         */
        virtual BIO_DM_NS::IConcentrations* getConcentrations();

        /**
         *  Updates concentrations at the current point.
         */
        virtual void setConcentrations(BIO_DM_NS::IConcentrations* source);

        /**
         *  Returns concentration of the substance in a current point.
         *  This is implementation of an interface IConcentrations.
         */
        virtual double getConcentration(int substanceNr);

        /**
         *  Sets the new value for substance concentration.
         *  This is implementation of an interface IConcentrations.
         */
        virtual void setConcentration(int substanceNr, double concentration);
    };

};



BIO_SLV_FD_IM2D_NS_END

#endif
