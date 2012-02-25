/*
 * Copyright 2012 Karolis Petrauskas
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
#ifndef BIO_ERS_ErlangMsgCodec_HXX
#define BIO_ERS_ErlangMsgCodec_HXX
#include <iostream>

/**
 *  Base class for all Erlang message encoders/decoders.
 */
class ErlangMsgCodec
{
protected:
    std::ostream *log;

public:
    /**
     *  Constructor.
     */
    ErlangMsgCodec();

    /**
     *  Destructor.
     */
    virtual ~ErlangMsgCodec();

    /**
     *  Encodes message to the Erlang format.
     */
    virtual bool encode() = 0;

    /**
     *  Decodes message from the Erlang format.
     */
    virtual bool decode(char *msgBuf, int msgLen) = 0;

    /**
     *  This will be invoked when all message info is already processed.
     *  The method sgould prepare codec for processing of next message.
     */
    virtual void cleanup() = 0;

    /**
     *  Sets stream for writing logs. The logging is disabled if 0 (null) is passed here.
     *  @param log Log stream or null, if logging should be disabled.
     *  @returns Previous logging stream or null if logging was disabled.
     */
    std::ostream* setLog(std::ostream* logStream);

protected:

    /**
     *  Throws Exception if eirc indicates an error.
     *  @param eirc return code from functions in the Erlang EI library.
     */
    void assertrc(int eirc);
};

#endif
