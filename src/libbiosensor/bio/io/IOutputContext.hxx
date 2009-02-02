#ifndef BIO_IO_IOutputContext_HXX
#define BIO_IO_IOutputContext_HXX
#include "../../biosensor.hxx"
#include <string>
#include <ostream>
#include <istream>
BIO_IO_NS_BEGIN


/**
 *
 */
class IOutputContext
{
public:

    /**
     *  Destructor.
     */
    virtual ~IOutputContext()
    {
        //  Empty virtual constructor.
    }

    /**
     *  Create or get existing output stream by name.
     *
     *  \param name Name for a destination.
     */
    virtual std::ostream* getOutputStream(std::string& name) = 0;

    /**
     *  Create or get existing indexed output stream by name.
     *
     *  \param name     Name for a destination.
     *  \param index    Index number for a destination.
     */
    virtual std::ostream* getOutputStream(std::string& name, long index) = 0;

    /**
     *  Create or get existing output stream by name.
     *
     *  \param name Name for a destination.
     */
    virtual std::istream* getInputStream(std::string& name) = 0;

    /**
     *  Create or get existing indexed output stream by name.
     *
     *  \param name     Name for a destination.
     *  \param index    Index number for a destination.
     */
    virtual std::istream* getInputStream(std::string& name, long index) = 0;

    /**
     *  Close specified stream.
     */
    virtual void close(std::ostream* stream) = 0;

    /**
     *  Close all opened streams.
     */
    virtual void close() = 0;


};



BIO_IO_NS_END
#endif
