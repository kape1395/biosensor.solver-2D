#ifndef BIO_IO_IRepeatable_HXX
#define BIO_IO_IRepeatable_HXX
#include "../../biosensor.hxx"
BIO_IO_NS_BEGIN


/**
 *  Interface for those output generators, which can be invoked multiple times.
 */
class IRepeatable
{
public:
    virtual ~IRepeatable()
    {
        // Empty virtual destructor.
    }

    virtual void setRepeatable(bool repeatable) = 0;
};



BIO_IO_NS_END
#endif
