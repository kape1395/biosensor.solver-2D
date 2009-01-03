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

public:

    /**
     *  Constructor.
     */
    DataModel(
        Solver* solver
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
    virtual BIO_XML_NS::model::Substance getSubstanceConf(int index);

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
     *
     */
    virtual BIO_DM_NS::ICursor2D* newGridCursor();

    
private:
    
    /**
     *  Cursor...
     */
    class Cursor : public BIO_DM_NS::ICursor2D
    {
    private:
        DataModel* dataModel;
        int h;
        int v;
        int sizeH;
        int sizeV;

    public:

        /**
         *
         */
        Cursor(DataModel* dataModel);

        /**
         *
         */
        virtual ~Cursor();

        virtual void left()
        {
            --h;
        }
        
        virtual void right()
        {
            h++;
        }
    
        virtual void top()
        {
            v--;
        }

        virtual void down()
        {
            v++;
        }

        virtual void rowStart()
        {
            h = 0;
        }

        virtual void rowEnd()
        {
            h = sizeH - 1;
        }

        virtual void colStart()
        {
            v = 0;
        }

        virtual void colEnd()
        {
            v = sizeV - 1;
        }

        virtual bool isValid()
        {
            return h >= 0 && h < sizeH && v >= 0 && v < sizeV;
        }

        virtual BIO_DM_NS::IConcentrations& getConcentrations();
    };

};



BIO_SLV_FD_IM2D_NS_END

#endif
