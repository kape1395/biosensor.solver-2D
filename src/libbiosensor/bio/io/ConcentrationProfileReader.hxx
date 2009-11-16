#ifndef BIO_IO_ConcentrationProfileReader_HXX
#define BIO_IO_ConcentrationProfileReader_HXX
#include "../../biosensor.hxx"
#include "../dm/IDataModel.hxx"
#include "../dm/IGrid2D.hxx"
#include "../dm/ICursor2D.hxx"
#include "../dm/ISegmentSplit.hxx"
#include "../dm/IConcentrations.hxx"
#include "../dm/AbstractCursor2D.hxx"
#include "../cfg/StructureAnalyzer.hxx"
#include "../slv/ISolverState.hxx"
#include "../Exception.hxx"
#include <boost/filesystem/path.hpp>
#include <boost/filesystem.hpp>
#include <string>
BIO_IO_NS_BEGIN


/**
 *  Reads concentrations from saved file. This can be used to restore
 *  solver state when resuming a simulation.
 *
 *  Implementation is not very clean...
 */
class ConcentrationProfileReader :
            public BIO_DM_NS::IGrid2D,
            public BIO_DM_NS::IDataModel,
            public BIO_SLV_NS::ISolverState
{
protected:
    class Cursor2DImpl;
    class SegmentSplitImpl;

    static const std::streamsize LINE_SIZE;
    static const std::string HDR_SOLVED_ITER;
    static const std::string HDR_SOLVED_TIME;
    static const std::string HDR_POS_H;
    static const std::string HDR_POS_V;
    static const std::string HDR_IDX_H;
    static const std::string HDR_IDX_V;

    /**
     *  Represents one line from the file.
     */
    struct DataPoint
    {
        double posH;
        double posV;
        int idxH;
        int idxV;
        double* substance;
    };
    std::vector<DataPoint> rawData;

private:
    boost::filesystem::path parsedFile;
    BIO_CFG_NS::StructureAnalyzer *structAnalyzer;

    long iterationNumber;
    double solvedTime;
    SegmentSplitImpl *pointsH;
    SegmentSplitImpl *pointsV;

    int colPosH;
    int colPosV;
    int colIdxH;
    int colIdxV;
    int columnCount;
    int *colSubstance;  ///< Mapping: colSubstance[globalSubstanceIndex] = columnInFile
    int *substIdxByCol; ///< Mapping: colSubstance[columnInFile] = globalSubstanceIndex

    int substanceCount;
    int sizeH;          ///< size of the matrix
    int sizeV;          ///< size of the matrix
    DataPoint** matrix; ///< rawData layed out in the matrix form.

    SegmentSplitImpl* pointPositionsH;
    SegmentSplitImpl* pointPositionsV;
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
     *  Copies the state from source to this data model.
     */
    virtual void setState(BIO_DM_NS::IDataModel *source)
    {
        throw Exception("setState is not supported in the ConcentrationProfileReader");
    }

    /**
     *  Returns solvedTime. This is implementation of ISolverState.
     */
    virtual double getTime()
    {
        if (solvedTime == -1.0)
            throw BIO_NS::Exception("Time is not defined...");
        return solvedTime;
    }

    /**
     *  Returns iteration number. This is implementation of ISolverState.
     */
    virtual long getIteration()
    {
        if (iterationNumber == -1)
            throw BIO_NS::Exception("Iteration is not defined...");
        return iterationNumber;
    }

    /**
     *  This is implementation of ISolverState.
     */
    virtual BIO_DM_NS::IDataModel* getData()
    {
        return this;
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
    /**
     *  Parses one header line.
     *  @return false, if this was not a header line (and was not parsed).
     */
    bool parseHeaderLine(std::string line);
    /**
     *  Parses one line of data (not a header).
     *  \return true, if line was processed or false if skipped (empty or comment).
     */
    bool parseDataLine(std::string line);
    /**
     *  Constructs matrix out of the rawData.
     */
    void constructMatrix();

    /* ********************************************************************** */
    /**
     *  Cursor impl.
     */
    class Cursor2DImpl : public BIO_DM_NS::AbstractCursor2D, public BIO_DM_NS::IConcentrations
    {
    private:
        ConcentrationProfileReader* reader;

    public:
        Cursor2DImpl(ConcentrationProfileReader* reader);
        virtual ~Cursor2DImpl();
        virtual BIO_DM_NS::IConcentrations *getConcentrations();
        virtual double getConcentration(int substanceNr);
    };

    /* ********************************************************************** */
    /**
     *  SegmentSplit impl.
     */
    class SegmentSplitImpl : public BIO_DM_NS::ISegmentSplit
    {
    private:
        std::vector<double> points;

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

        void append(double point);
        void reset();
    };
    /* ********************************************************************** */
};



BIO_IO_NS_END
#endif
