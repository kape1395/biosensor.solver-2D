#ifndef BIO_IO_FilesystemOutputContext_HXX
#define BIO_IO_FilesystemOutputContext_HXX
#include "../../biosensor.hxx"
#include "IOutputContext.hxx"
#include <string>
#include <ostream>
#include <istream>
BIO_IO_NS_BEGIN


/**
 *
 */
class FilesystemOutputContext : public IOutputContext
{
public:

    /**
     *  Constructor.
     *
     *  \param destDir  Destination directory.
     */
    FilesystemOutputContext(std::string& destDir);

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

};



BIO_IO_NS_END
#endif
