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

    std::map<std::string, std::ofstream*> openOStreams;
    std::vector<std::ifstream*> openIStreams;

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
     *  Save original configuration in the context.
     *
     *  \param config Stream, having configuration.
     */
    virtual void setOriginalConfiguration(std::istream& config);

    /**
     *  Save actual configuration in the context.
     *
     *  \param config Stream, having configuration.
     */
    virtual void setActualConfiguration(std::istream& config);

    /**
     *  Save configuration in the context.
     *
     *  \param config Stream, having configuration.
     */
    virtual void setConfiguration(std::istream& config, std::string& name);

    /**
     *  Create or get existing output stream by name.
     *
     *  \param name Name for a destination.
     */
    virtual std::ostream* getOutputStream(const std::string& name, bool overwrite = false);

    /**
     *  Create or get existing indexed output stream by name.
     *
     *  \param name     Name for a destination.
     *  \param index    Index number for a destination.
     */
    virtual std::ostream* getOutputStream(const std::string& name, long index, bool overwrite = false);

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
     *  Close specified stream.
     */
    virtual void close(std::istream* stream);

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
