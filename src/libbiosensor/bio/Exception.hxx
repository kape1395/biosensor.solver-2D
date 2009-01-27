#ifndef BIO_Exception_HXX
#define BIO_Exception_HXX
#include "../biosensor.hxx"
#include <string>
#include <stdexcept>
BIO_NS_BEGIN


/**
 *  Bazinis exceptionas sitame softe.
 */
class Exception : public std::logic_error
{
protected:
    std::string message;

public:
    Exception(const std::string& message);
    virtual ~Exception() throw();

};


BIO_NS_END
#endif
