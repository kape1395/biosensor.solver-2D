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
#include "Solver.hxx"
#include <log4cxx/logger.h>

BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *  DataModel facade for users of Solver class.
 */
class DataModel : public BIO_DM_NS::IDataModel, public BIO_DM_NS::IGrid2D
{
private:
    Solver* solver;
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer;
    int *areaRangesH;   // Point number, at which starts h^th area.
    int *areaRangesV;   // Point number, at which starts v^th area.
    int areaCountH;
    int areaCountV;
    int pointCountH;    // Total count of points in horizontal axis.
    int pointCountV;    // Total count of points in vertical axis.


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
    virtual int getPointCountH();

    /**
     *
     */
    virtual int getPointCountV();

    /**
     *
     */
    virtual double* getPointPositionsH();

    /**
     *
     */
    virtual double* getPointPositionsV();

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
class Cursor : public BIO_DM_NS::ICursor2D, public BIO_DM_NS::IConcentrations
    {
    private:
        DataModel* dataModel;
        int sizeH;      // Total size H
        int sizeV;      // Total size V
        int currentH;   // point index in H
        int currentV;   // point index in V;
        int currentAreaH;
        int currentAreaV;
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

        virtual void left();
        virtual void right();
        virtual void top();
        virtual void down();

        virtual void rowStart();
        virtual void rowEnd();
        virtual void colStart();
        virtual void colEnd();

        virtual bool isValid();

        virtual BIO_DM_NS::IConcentrations* getConcentrations();

        /**
         *  Returns concentration of the substance in a current point.
         *  This is implementation of an interface IConcentrations.
         */
        virtual double operator [] (int substanceNr);

    };

};



BIO_SLV_FD_IM2D_NS_END

#endif
