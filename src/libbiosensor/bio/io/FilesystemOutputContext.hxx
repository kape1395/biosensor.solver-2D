#ifndef BIO_IO_FilesystemOutputContext_HXX
#define BIO_IO_FilesystemOutputContext_HXX
#include "../../biosensor.hxx"
#include "IOutputContext.hxx"
#include <string>
#include <ostream>
#include <istream>
#include <vector>
BIO_IO_NS_BEGIN


/**
 *
 */
class FilesystemOutputContext : public IOutputContext
{
private:
    std::string baseDir;
    std::vector<std::string> fileNames;
    std::vector<std::ostream*> openOStreams;
    std::vector<std::istream*> openIStreams;

public:

    /**
     *  Constructor.
     *
     *  \param destDir  Destination directory.
     */
    FilesystemOutputContext(std::string& baseDir);

    /**
     *  Destructor.
     */
    virtual ~FilesystemOutputContext();

    /**
     *  Create or get existing output stream by name.
     *
     *  \param name Name for a destination.
     */
    virtual std::ostream* getOutputStream(std::string& name);

    /**
     *  Create or get existing indexed output stream by name.
     *
     *  \param name     Name for a destination.
     *  \param index    Index number for a destination.
     */
    virtual std::ostream* getOutputStream(std::string& name, long index);

    /**
     *  Create or get existing output stream by name.
     *
     *  \param name Name for a destination.
     */
    virtual std::istream* getInputStream(std::string& name);

    /**
     *  Create or get existing indexed output stream by name.
     *
     *  \param name     Name for a destination.
     *  \param index    Index number for a destination.
     */
    virtual std::istream* getInputStream(std::string& name, long index);

    /**
     *  Close specified stream.
     */
    virtual void close(std::ostream* stream);

    /**
     *  Close all opened streams.
     */
    virtual void close();

protected:
    std::string getFileName(std::string& name);
    std::string getFileName(std::string& name, long index);

};



BIO_IO_NS_END
#endif
