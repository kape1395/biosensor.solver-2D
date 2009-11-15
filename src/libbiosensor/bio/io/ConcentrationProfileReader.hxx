#ifndef BIO_IO_ConcentrationProfileReader_HXX
#define BIO_IO_ConcentrationProfileReader_HXX
#include "../../biosensor.hxx"
#include "../dm/IGrid2D.hxx"
#include "../dm/ICursor2D.hxx"
#include "../dm/ISegmentSplit.hxx"
#include "../dm/IConcentrations.hxx"
#include "../dm/AbstractCursor2D.hxx"
#include <boost/filesystem/path.hpp>
#include <boost/filesystem.hpp>
#include <string>
BIO_IO_NS_BEGIN


/**
 *  Reads concentrations from saved file. This can be used to restore
 *  solver state when resuming a simulation.
 */
class ConcentrationProfileReader : public BIO_DM_NS::IGrid2D
{
protected:
    class Cursor2DImpl;
    class SegmentSplitImpl;

private:
    boost::filesystem::path parsedFile;
    long iterationNumber;
    double solvedTime;
    SegmentSplitImpl *pointsH;
    SegmentSplitImpl *pointsV;

public:
    /**
     *  Constructor.
     */
    ConcentrationProfileReader(
        BIO_XML_MODEL_NS::Model* model,
        boost::filesystem::path& concentrationsFile
    );

    /**
     *  Destructor.
     */
    virtual ~ConcentrationProfileReader();

    /**
     *  Returns iteration number, if it was found from the file.
     *  @return iteration number or -1, if it was not found.
     */
    virtual long getIterationNumber()
    {
        return iterationNumber;
    }

    /**
     *  Returns solved time, if it was found from the file.
     *  @return solved time or -1, if it was not found.
     */
    virtual double getSolvedTime()
    {
        return solvedTime;
    }

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
    /**
     *  Parses specified file and fills all internal structures.
     */
    void parse(boost::filesystem::path& concentrationsFile);

    /* ********************************************************************** */
    /**
     *  Cursor impl.
     */
    class Cursor2DImpl : public BIO_DM_NS::AbstractCursor2D
    {
    private:
        ConcentrationProfileReader* reader;

    public:
        Cursor2DImpl(ConcentrationProfileReader* reader);
        virtual ~Cursor2DImpl();
        virtual BIO_DM_NS::IConcentrations *getConcentrations();
    };

    /* ********************************************************************** */
    /**
     *  SegmentSplit impl.
     */
    class SegmentSplitImpl : public BIO_DM_NS::ISegmentSplit
    {
    public:
        SegmentSplitImpl();
        virtual ~SegmentSplitImpl();
        virtual double getLength();
        virtual double getLocalPointPosition(int i);
        virtual int getPointCount();
        virtual double getPointPosition(int i);
        virtual double getStartPosition();
        virtual int getStepCount();
        virtual double getStepSize(int i);
    };
    /* ********************************************************************** */
};



BIO_IO_NS_END
#endif
