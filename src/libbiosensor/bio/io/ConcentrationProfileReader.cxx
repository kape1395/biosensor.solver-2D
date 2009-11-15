#include "ConcentrationProfileReader.hxx"
#include "../Exception.hxx"
#include "../Logging.hxx"
#include "../dm/ISegmentSplit.hxx"
#include <iostream>
#include <sstream>
#include <cmath>
#include <fstream>
#define LOGGER "libbiosensor::ConcentrationProfileReader: "

/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::ConcentrationProfileReader::ConcentrationProfileReader(
    BIO_XML_MODEL_NS::Model* model,
    boost::filesystem::path& concentrationsFile
)
{
    parse(concentrationsFile);
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::ConcentrationProfileReader::~ConcentrationProfileReader()
{
    //  TODO: Implement.
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_IO_NS::ConcentrationProfileReader::parse(boost::filesystem::path& concentrationsFile)
{
    this->parsedFile = concentrationsFile;
    this->iterationNumber = -1;
    this->solvedTime = -1.0;

    std::ifstream input;
    input.exceptions(std::ifstream::badbit);

    const std::streamsize LINE_SIZE = 1000;
    const std::string SOLVED_ITER("SolvedIterationCount");
    const std::string SOLVED_TIME("SolvedTime");

    char lineBuf[LINE_SIZE];

    bool header = true; // true, if we are reading a header.
    try
    {
        input.open(concentrationsFile.file_string().c_str(), std::ios::in);
        while (!input.eof())
        {
            input.getline(lineBuf, LINE_SIZE-1);
            if (input.gcount() == 0 && input.eof())
            {
                break;  // last empty line was read.
            }
            std::string line(lineBuf);
            if (header)
            {
                if (line.length() == 0)
                {
                    //  Skipping empty line.
                }
                else if (line[0] == '#')
                {
                    std::size_t posFrom;
                    std::size_t posTo;
                    //
                    //  Find SOLVED_ITER
                    if ((posFrom = line.find(SOLVED_ITER)) != std::string::npos && line[posFrom + SOLVED_ITER.length()] == '=')
                    {
                        posFrom = posFrom + SOLVED_ITER.size() + 1;
                        for (posTo = posFrom; posTo < line.length() && line[posTo] != ' ' && line[posTo] != '\t'; posTo++);
                        std::string tmp = line.substr(posFrom, posTo - posFrom);
                        LOG_DEBUG(LOGGER << "Found SOLVED_ITER=\"" << tmp << "\"");
                    }
                    //
                    //  Find SOLVED_TIME
                    if ((posFrom = line.find(SOLVED_TIME)) != std::string::npos && line[posFrom + SOLVED_TIME.length()] == '=')
                    {
                        posFrom = posFrom + SOLVED_TIME.size() + 1;
                        for (posTo = posFrom; posTo < line.length() && line[posTo] != ' ' && line[posTo] != '\t'; posTo++);
                        std::string tmp = line.substr(posFrom, posTo - posFrom);
                        LOG_DEBUG(LOGGER << "Found SOLVED_TIME=\"" << tmp << "\"");
                    }
                    //
                    //  Find column headers...
                    if (line.find("pos_h") != std::string::npos)
                    {   // column descriptions
                        std::istringstream l(line);
                        while (!l.eof())
                        {
                            std::string word;
                            l >> word;
                            LOG_DEBUG(LOGGER << "W: " << word << "-");
                        }
                    }
                }
                else
                {
                    header = false;
                }
            }

            if (!header)
            {
                if (line.length() == 0)
                {
                    //  Skipping empty line.
                }
                //LOG_DEBUG(LOGGER << "DATA: " << line);
            }
        }
        input.close();
    }
    catch (std::ifstream::failure e)
    {
        input.close();
        LOG_ERROR(LOGGER << "Failed to parse concentrations file: " << e.what());
        throw BIO_NS::Exception("Failed to parse concentrations file");
    }

}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_DM_NS::ISegmentSplit* BIO_IO_NS::ConcentrationProfileReader::getPointPositionsH()
{
    //  TODO: Implement.
    return 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_DM_NS::ISegmentSplit* BIO_IO_NS::ConcentrationProfileReader::getPointPositionsV()
{
    //  TODO: Implement.
    return 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_DM_NS::ICursor2D* BIO_IO_NS::ConcentrationProfileReader::newGridCursor()
{
    //  TODO: Implement.
    return 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
int BIO_IO_NS::ConcentrationProfileReader::getSubstanceCount()
{
    //  TODO: Implement.
    return 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_XML_NS::model::Substance* BIO_IO_NS::ConcentrationProfileReader::getSubstanceConf(int index)
{
    //  TODO: Implement.
    return 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::ConcentrationProfileReader::Cursor2DImpl::Cursor2DImpl(ConcentrationProfileReader* reader) :
        BIO_DM_NS::AbstractCursor2D(0, 0)
{
    //  TODO: Implement.
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::ConcentrationProfileReader::Cursor2DImpl::~Cursor2DImpl()
{
    // Nothing
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_DM_NS::IConcentrations *BIO_IO_NS::ConcentrationProfileReader::Cursor2DImpl::getConcentrations()
{
    //  TODO: Implement.
    return 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::ConcentrationProfileReader::SegmentSplitImpl::SegmentSplitImpl()
{
    //  TODO: Implement.

}

/* ************************************************************************** */
BIO_IO_NS::ConcentrationProfileReader::SegmentSplitImpl::~SegmentSplitImpl()
{
    //  TODO: Implement.

}

/* ************************************************************************** */
double BIO_IO_NS::ConcentrationProfileReader::SegmentSplitImpl::getLength()
{
    //  TODO: Implement.

}

/* ************************************************************************** */
double BIO_IO_NS::ConcentrationProfileReader::SegmentSplitImpl::getLocalPointPosition(int i)
{
    //  TODO: Implement.

}

/* ************************************************************************** */
int BIO_IO_NS::ConcentrationProfileReader::SegmentSplitImpl::getPointCount()
{
    //  TODO: Implement.

}

/* ************************************************************************** */
double BIO_IO_NS::ConcentrationProfileReader::SegmentSplitImpl::getPointPosition(int i)
{
    //  TODO: Implement.

}

/* ************************************************************************** */
double BIO_IO_NS::ConcentrationProfileReader::SegmentSplitImpl::getStartPosition()
{
    //  TODO: Implement.

}

/* ************************************************************************** */
int BIO_IO_NS::ConcentrationProfileReader::SegmentSplitImpl::getStepCount()
{
    //  TODO: Implement.

}

/* ************************************************************************** */
double BIO_IO_NS::ConcentrationProfileReader::SegmentSplitImpl::getStepSize(int i)
{
    //  TODO: Implement.

}


/* ************************************************************************** */
/* ************************************************************************** */
