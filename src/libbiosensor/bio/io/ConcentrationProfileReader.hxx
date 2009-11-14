#ifndef BIO_IO_ConcentrationProfileReader_HXX
#define BIO_IO_ConcentrationProfileReader_HXX
#include "../../biosensor.hxx"
#include "../dm/IGrid2D.hxx"
#include "../dm/ICursor2D.hxx"
#include <boost/filesystem/path.hpp>
#include <boost/filesystem.hpp>
#include <string>
BIO_IO_NS_BEGIN


/**
 *
 */
class ConcentrationProfileReader
{
private:
    boost::filesystem::path parsedFile;
    long iterationNumber;
    double solvedTime;

public:
    /**
     *  Constructor.
     */
    ConcentrationProfileReader(
        boost::filesystem::path& concentrationsFile
    );

    /**
     *  Destructor.
     */
    virtual ~ConcentrationProfileReader();

protected:
    /**
     *  Parses specified file and fills all internal structures.
     */
    void parse(boost::filesystem::path& concentrationsFile);

};



BIO_IO_NS_END
#endif
