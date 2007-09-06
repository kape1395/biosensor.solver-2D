#ifndef Exceptions_HH
#define Exceptions_HH
#include <string>


class Exception
{
protected:
    std::string msg;

public:
    Exception(std::string msg)
    {
        this->msg = msg;
    }
    std::string& message()
    {
        return msg;
    }
};


#endif  // Exception_HH
