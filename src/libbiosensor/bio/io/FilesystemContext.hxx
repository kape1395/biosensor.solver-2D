#ifndef BIO_IO_FilesystemContext_HXX
#define BIO_IO_FilesystemContext_HXX
#include "../../biosensor.hxx"
#include "IContext.hxx"
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
class FilesystemContext : public IContext
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
    FilesystemContext(const std::string& baseDir);

    /**
     *  Destructor.
     */
    virtual ~FilesystemContext();

    /**
     *  Save configuration in the context.
     *
     *  \param config Stream, having configuration.
     */
    virtual void setConfiguration(std::istream& config);

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
