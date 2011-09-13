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
#include "ConcentrationProfileReader.hxx"
#include "../Exception.hxx"
#include "../Logging.hxx"
#include "../dm/ISegmentSplit.hxx"
#include "../cfg/StructureAnalyzer.hxx"
#include <iostream>
#include <sstream>
#include <cmath>
#include <fstream>
#include <vector>
#define LOGGER "libbiosensor::ConcentrationProfileReader: "


const std::streamsize BIO_IO_NS::ConcentrationProfileReader::LINE_SIZE = 1000;
const std::string BIO_IO_NS::ConcentrationProfileReader::HDR_SOLVED_ITER = "SolvedIterationCount";
const std::string BIO_IO_NS::ConcentrationProfileReader::HDR_SOLVED_TIME = "SolvedTime";
const std::string BIO_IO_NS::ConcentrationProfileReader::HDR_POS_H = "pos_h";
const std::string BIO_IO_NS::ConcentrationProfileReader::HDR_POS_V = "pos_v";
const std::string BIO_IO_NS::ConcentrationProfileReader::HDR_IDX_H = "idx_h";
const std::string BIO_IO_NS::ConcentrationProfileReader::HDR_IDX_V = "idx_v";


/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::ConcentrationProfileReader::ConcentrationProfileReader(
    BIO_XML_MODEL_NS::Model* model,
    std::istream& input
)
{
    structAnalyzer = new BIO_CFG_NS::StructureAnalyzer(model);
    substanceCount = structAnalyzer->getSubstances().size();
    colSubstance = new int[substanceCount];
    parse(input);
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::ConcentrationProfileReader::~ConcentrationProfileReader()
{
    delete structAnalyzer;
    delete[] colSubstance;
    delete[] substIdxByCol;

    for (std::vector<DataPoint>::iterator dp = rawData.begin(); dp < rawData.end(); dp++)
    {
        delete[] dp->substance;
    }
    rawData.clear();

    for (int h = 0; h < sizeH; h++)
    {
        delete[] matrix[h];
    }
    delete[] matrix;

    delete pointPositionsH;
    delete pointPositionsV;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_IO_NS::ConcentrationProfileReader::parse(std::istream& input)
{
    this->iterationNumber = -1;
    this->solvedTime = -1.0;
    this->colPosH = -1;
    this->colPosV = -1;
    this->colIdxH = -1;
    this->colIdxV = -1;
    for (int i = 0; i < substanceCount; i++)
    {
        colSubstance[i] = -1;
    }


    char lineBuf[LINE_SIZE];

    bool header = true; // true, if we are reading a header.
    try
    {
        LOG_DEBUG(LOGGER << "Parsing a header...");
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
                header = parseHeaderLine(line);
                if (!header) // Parsing of header is done.
                {
                    LOG_DEBUG(LOGGER << "Parsing a header... Done:"
                              << " iterationNumber=" << iterationNumber
                              << " solvedTime=" << solvedTime
                              << " colPosH=" << colPosH
                              << " colPosV=" << colPosV
                              << " colIdxH=" << colIdxH
                              << " colIdxV=" << colIdxV
                              << " columnCount=" << columnCount
                             );

                    // Initialize following mapping:
                    substIdxByCol = new int[columnCount];
                    for (int i = 0; i < columnCount; i++)
                        substIdxByCol[i] = -1;

                    // Check if columns for all concentrations were found.
                    // And fill substIdxByCol mapping.
                    for (int i = 0; i < substanceCount; i++)
                    {
                        if (colSubstance[i] == -1)
                        {
                            LOG_ERROR(LOGGER << "Column not found for substance \""
                                      << structAnalyzer->getSubstances()[i]
                                      << "\" in the concentrations file"
                                     );
                            throw BIO_NS::Exception("Column not found for substance in the concentrations file");
                        }
                        substIdxByCol[colSubstance[i]] = i;
                    }
                }
            }
            if (!header)
            {
                parseDataLine(line);
            }
        }
    }
    catch (std::ifstream::failure e)
    {
        LOG_ERROR(LOGGER << "Failed to parse concentrations file: " << e.what());
        throw BIO_NS::Exception("Failed to parse concentrations file");
    }

    //
    //  Layout all data in the matrix form.
    //
    constructMatrix();

    //
    //  construct point positions
    //
    pointPositionsH = new SegmentSplitImpl();
    for (int h = 0; h < sizeH; h++)
        pointPositionsH->append(matrix[h][0].posH);

    pointPositionsV = new SegmentSplitImpl();
    for (int v = 0; v < sizeV; v++)
        pointPositionsV->append(matrix[0][v].posV);

}


/* ************************************************************************** */
/* ************************************************************************** */
bool BIO_IO_NS::ConcentrationProfileReader::parseHeaderLine(std::string line)
{
    if (line.length() == 0)
    {
        //  Skipping empty line.
        return true;
    }
    else if (line[0] == '#')
    {
        // processing follows.
    }
    else
    {
        //  Not a header line.
        return false;
    }


    std::size_t posFrom;
    std::size_t posTo;

    //
    //  Find SOLVED_ITER
    //
    if ((posFrom = line.find(HDR_SOLVED_ITER)) != std::string::npos && line[posFrom + HDR_SOLVED_ITER.length()] == '=')
    {
        posFrom = posFrom + HDR_SOLVED_ITER.size() + 1;
        for (posTo = posFrom; posTo < line.length() && line[posTo] != ' ' && line[posTo] != '\t'; posTo++);
        std::string tmp = line.substr(posFrom, posTo - posFrom);
        std::stringstream tmpss(tmp);
        tmpss >> iterationNumber;
        LOG_TRACE(LOGGER << "Found SOLVED_ITER=\"" << tmp << "\"=" << iterationNumber);
    }

    //
    //  Find SOLVED_TIME
    //
    if ((posFrom = line.find(HDR_SOLVED_TIME)) != std::string::npos && line[posFrom + HDR_SOLVED_TIME.length()] == '=')
    {
        posFrom = posFrom + HDR_SOLVED_TIME.size() + 1;
        for (posTo = posFrom; posTo < line.length() && line[posTo] != ' ' && line[posTo] != '\t'; posTo++);
        std::string tmp = line.substr(posFrom, posTo - posFrom);
        std::stringstream tmpss(tmp);
        tmpss >> solvedTime;
        LOG_TRACE(LOGGER << "Found SOLVED_TIME=\"" << tmp << "\"=" << solvedTime);
    }

    //
    //  Find column headers...
    //
    if (line.find(HDR_POS_H) != std::string::npos)
    {   // column descriptions
        std::istringstream lineStream(line);
        std::string word;
        int col = 0;
        while (!lineStream.eof())
        {
            lineStream >> word;
            if (word.compare("#") == 0 && col == 0)
            {
                //  skip comment sign.
            }
            else if (word.compare(HDR_POS_H) == 0)
            {
                colPosH = col++;
            }
            else if (word.compare(HDR_POS_V) == 0)
            {
                colPosV = col++;
            }
            else if (word.compare(HDR_IDX_H) == 0)
            {
                colIdxH = col++;
            }
            else if (word.compare(HDR_IDX_V) == 0)
            {
                colIdxV = col++;
            }
            else
            {
                //  Subsrance concentration.
                LOG_TRACE(LOGGER << "Found header of column for substance: " << word);

                BIO_XML_MODEL_NS::SubstanceName substanceName(word);
                int globalSubstanceIndex = structAnalyzer->getSubstanceIndex(substanceName);
                colSubstance[globalSubstanceIndex] = col++;

                LOG_DEBUG(LOGGER << "Substance \"" << word
                          << "\" will be read from column " << colSubstance[globalSubstanceIndex]
                          << " and mapped to global substance index "
                          << globalSubstanceIndex
                         );
            }
        }
        columnCount = col;
    }   //  Find column headers...

    return true;
}


/* ************************************************************************** */
/* ************************************************************************** */
bool BIO_IO_NS::ConcentrationProfileReader::parseDataLine(std::string line)
{
    LOG_TRACE(LOGGER << "Parsing data line: " << line);
    if (line.length() == 0 || line[0] == '#')
    {
        //  Skipping empty line or a comment
        return false;
    }

    DataPoint dataPoint;
    dataPoint.substance = new double[substanceCount];

    std::stringstream lineStream(line);
    int col;
    for (col = 0; col < columnCount; col++)
    {
        if (col == colPosH)
            lineStream >> dataPoint.posH;
        else if (col == colPosV)
            lineStream >> dataPoint.posV;
        else if (col == colIdxH)
            lineStream >> dataPoint.idxH;
        else if (col == colIdxV)
            lineStream >> dataPoint.idxV;
        else
        {
            lineStream >> dataPoint.substance[substIdxByCol[col]];
            if (lineStream.fail())
            {
                lineStream.clear();
                std::string value;
                lineStream >> value;
                if (value == "nan" || value == "NaN" || value == "NAN")
                {
                    //  OK, that's NAN
                    dataPoint.substance[substIdxByCol[col]] = NAN;
                }
                else
                {
                    LOG_ERROR(LOGGER << "Found invalid substance concentration: " << value);
                    throw Exception("ConcentrationProfileReader found invalid substance concentration");
                }
            }
        }
    }

    if (lineStream.bad() || lineStream.fail())
        throw BIO_NS::Exception("ConcentrationProfileReader failed to read data (parse a data line)");


    //
    //  Write all this to log.
    //
    std::stringstream log_subst;
    for (int i = 0; i < substanceCount; i++)
        log_subst << " " << dataPoint.substance[i];
    LOG_TRACE(LOGGER << "Data point was read:"
              << " posH=" << dataPoint.posH
              << " posV=" << dataPoint.posV
              << " idxH=" << dataPoint.idxH
              << " idxV=" << dataPoint.idxV
              << " substances:" << log_subst.str()
             );

    //
    //  Done.
    //
    rawData.push_back(dataPoint);
    return true;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_IO_NS::ConcentrationProfileReader::constructMatrix()
{
    std::vector<DataPoint>::iterator raw;

    //
    //  Get matrix size
    //
    sizeH = 0;
    sizeV = 0;
    for (raw = rawData.begin(); raw < rawData.end(); raw++)
    {
        sizeH = (sizeH < raw->idxH) ? raw->idxH : sizeH;
        sizeV = (sizeV < raw->idxV) ? raw->idxV : sizeV;
    }
    sizeH++;
    sizeV++;

    LOG_DEBUG(LOGGER << "Constructing matrix with sizeH=" << sizeH << " sizeV=" << sizeV);

    //
    //  Allocate memory
    //
    matrix = new DataPoint*[sizeH];
    for (int h = 0; h < sizeH; h++)
    {
        matrix[h] = new DataPoint[sizeV];
    }

    //
    //  Fill...
    //
    for (raw = rawData.begin(); raw < rawData.end(); raw++)
    {
        matrix[raw->idxH][raw->idxV] = *raw;
    }

    LOG_DEBUG(LOGGER << "Constructing matrix - done");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_DM_NS::ISegmentSplit* BIO_IO_NS::ConcentrationProfileReader::getPointPositionsH()
{
    return pointPositionsH;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_DM_NS::ISegmentSplit* BIO_IO_NS::ConcentrationProfileReader::getPointPositionsV()
{
    return pointPositionsV;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_DM_NS::ICursor2D* BIO_IO_NS::ConcentrationProfileReader::newGridCursor()
{
    return new Cursor2DImpl(this);
}


/* ************************************************************************** */
/* ************************************************************************** */
int BIO_IO_NS::ConcentrationProfileReader::getSubstanceCount()
{
    return substanceCount;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_XML_NS::model::Substance* BIO_IO_NS::ConcentrationProfileReader::getSubstanceConf(int index)
{
    return structAnalyzer->getSubstances()[index];
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::ConcentrationProfileReader::Cursor2DImpl::Cursor2DImpl(ConcentrationProfileReader* reader) :
        BIO_DM_NS::AbstractCursor2D(
            reader->sizeH,
            reader->sizeV
        )
{
    this->reader = reader;
}

/* ************************************************************************** */
BIO_IO_NS::ConcentrationProfileReader::Cursor2DImpl::~Cursor2DImpl()
{
    // Nothing
}

/* ************************************************************************** */
BIO_DM_NS::IConcentrations *BIO_IO_NS::ConcentrationProfileReader::Cursor2DImpl::getConcentrations()
{
    return this;
}

/* ************************************************************************** */
double BIO_IO_NS::ConcentrationProfileReader::Cursor2DImpl::getConcentration(int substanceNr)
{
    return reader->matrix[currentH][currentV].substance[substanceNr];
}

/* ************************************************************************** */
void BIO_IO_NS::ConcentrationProfileReader::Cursor2DImpl::setConcentration(int substanceNr, double concentration)
{
    throw BIO_NS::Exception("setConcentration is not supported by ConcentrationProfileReader::Cursor2DImpl");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::ConcentrationProfileReader::SegmentSplitImpl::SegmentSplitImpl()
{
    points.clear();
}

/* ************************************************************************** */
BIO_IO_NS::ConcentrationProfileReader::SegmentSplitImpl::~SegmentSplitImpl()
{
    points.clear();
}

/* ************************************************************************** */
double BIO_IO_NS::ConcentrationProfileReader::SegmentSplitImpl::getLength()
{
    return points[points.size() - 1];
}

/* ************************************************************************** */
double BIO_IO_NS::ConcentrationProfileReader::SegmentSplitImpl::getLocalPointPosition(int i)
{
    return getPointPosition(i);
}

/* ************************************************************************** */
int BIO_IO_NS::ConcentrationProfileReader::SegmentSplitImpl::getPointCount()
{
    return points.size();
}

/* ************************************************************************** */
double BIO_IO_NS::ConcentrationProfileReader::SegmentSplitImpl::getPointPosition(int i)
{
    return points[i];
}

/* ************************************************************************** */
double BIO_IO_NS::ConcentrationProfileReader::SegmentSplitImpl::getStartPosition()
{
    return 0.0;
}

/* ************************************************************************** */
int BIO_IO_NS::ConcentrationProfileReader::SegmentSplitImpl::getStepCount()
{
    return points.size() - 1;
}

/* ************************************************************************** */
double BIO_IO_NS::ConcentrationProfileReader::SegmentSplitImpl::getStepSize(int i)
{
    return points[i + 1] - points[i];
}

/* ************************************************************************** */
void BIO_IO_NS::ConcentrationProfileReader::SegmentSplitImpl::append(double point)
{
    points.push_back(point);
}

/* ************************************************************************** */
void BIO_IO_NS::ConcentrationProfileReader::SegmentSplitImpl::reset()
{
    points.clear();
}


/* ************************************************************************** */
/* ************************************************************************** */
