#ifndef BIO_IO_FilesystemOutputContext_HXX
#define BIO_IO_FilesystemOutputContext_HXX
#include "../../biosensor.hxx"
#include "IOutputContext.hxx"
#include <map>
#include <string>
#include <ostream>
#include <istream>
#include <fstream>
#include <vector>
#include <boost/filesystem.hpp>
BIO_IO_NS_BEGIN


/**
 *
 */
class FilesystemOutputContext : public IOutputContext
{
private:
    boost::filesystem::path baseDirPath;
    std::vector<std::string> fileNames;
    //std::vector<std::ostream*> openOStreams;
    //std::vector<std::istream*> openIStreams;
    std::map<std::string, std::ofstream*> openOStreams;

public:

    /**
     *  Constructor.
     *
     *  \param destDir  Destination directory.
     */
    FilesystemOutputContext(const std::string& baseDir);

    /**
     *  Destructor.
     */
    virtual ~FilesystemOutputContext();

    /**
     *  Create or get existing output stream by name.
     *
     *  \param name Name for a destination.
     */
    virtual std::ostream* getOutputStream(const std::string& name);

    /**
     *  Create or get existing indexed output stream by name.
     *
     *  \param name     Name for a destination.
     *  \param index    Index number for a destination.
     */
    virtual std::ostream* getOutputStream(const std::string& name, long index);

    /**
     *  Create or get existing output stream by name.
     *
     *  \param name Name for a destination.
     */
    virtual std::istream* getInputStream(const std::string& name);

    /**
     *  Create or get existing indexed output stream by name.
     *
     *  \param name     Name for a destination.
     *  \param index    Index number for a destination.
     */
    virtual std::istream* getInputStream(const std::string& name, long index);

    /**
     *  Close specified stream.
     */
    virtual void close(std::ostream* stream);

    /**
     *  Close all opened streams.
     */
    virtual void close();

protected:

    /**
     *
     */
    boost::filesystem::path getFilePath(const std::string& name) const;

    /**
     *
     */
    std::string createIndexedFileName(const std::string& name, long index) const;

};



BIO_IO_NS_END
#endif
