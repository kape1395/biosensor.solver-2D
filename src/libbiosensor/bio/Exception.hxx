#ifndef BIO_Exception_HXX
#define BIO_Exception_HXX
#include "../biosensor.hxx"
#include <string>
#include <exception>
#include <iostream>
#include <sstream>
BIO_NS_BEGIN


/**
 *  Bazinis exceptionas sitame softe.
 */
class Exception : public std::exception //, public std::ostringstream
{
protected:
    std::ostringstream *msg;
    
public:
    Exception();
    Exception(std::string string);
    ~Exception() throw();
    virtual const char* what() const throw();
    
};


BIO_NS_END
#endif
