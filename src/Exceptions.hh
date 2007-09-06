#ifndef Exceptions_HH
#define Exceptions_HH
#include <string>
#include <exception>
#include <iostream>
#include <sstream>

/**
 *  Bazinis exceptionas sitame softe.
 */
class Exception : public std::exception //, public std::ostringstream
{
    /*
    friend Exception& operator << (Exception&, int);
    friend Exception& operator << (Exception&, double);
    friend Exception& operator << (Exception&, const std::string&);
    */
protected:
    std::ostringstream *msg;

public:
    Exception()
    {
        msg = new std::ostringstream();
    }
    /// Konstruktorius
    /// \param msg  Exception`o tekstas.
    Exception(std::string string)
    {
        msg = new std::ostringstream();
        *msg << string;
    }

    /// Tuscias destruktorius.
    ~Exception() throw()
    {
        delete msg;
    }

    /// Grazina exception`o pranesima.
    virtual const char* what() const throw()
    {
        return msg->str().c_str();
    }

};
/*
Exception& operator << (Exception& e, int i);
Exception& operator << (Exception& e, double d);
Exception& operator << (Exception& e, const std::string& s);
*/

/*
class msg {
public:
    std::string& format(std::string, ...)
    {}
}
*/

#endif  // Exception_HH
