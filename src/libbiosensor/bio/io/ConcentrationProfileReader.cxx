#include "ConcentrationProfileReader.hxx"
#include "../Exception.hxx"
#include "../dm/ISegmentSplit.hxx"
#include <iostream>
#include <sstream>
#include <cmath>
#include <c++/4.3/fstream>

/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::ConcentrationProfileReader::ConcentrationProfileReader(
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
    this->solvedTime = NAN;

    std::filebuf inputBuf;
    inputBuf.open(concentrationsFile.file_string().c_str(), std::ios::in);
    std::istream input(&inputBuf);

    const std::streamsize LINE_SIZE = 1000;
    char lineBuf[LINE_SIZE];

    bool header = true; // true, if we are reading a header.
    while (input.getline(lineBuf, LINE_SIZE-1), !input.eof() && !input.fail())
    {
        std::string line(lineBuf);
        if (header)
        {
            if (line[0] == '#')
            {

            }
            else
            {
                header = false;
            }
        }
        
        if (!header)
        {
            
        }
    }

    
    if (input.fail())
    {
        inputBuf.close();
        throw BIO_NS::Exception("Some error occured while parsing concentrations file");
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
