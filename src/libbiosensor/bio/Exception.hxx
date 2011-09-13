/*
 * Copyright 2011 Karolis Petrauskas
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
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
