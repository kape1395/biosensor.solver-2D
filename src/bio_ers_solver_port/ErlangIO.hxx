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
#ifndef BIO_ERS_ErlangIO_HXX
#define BIO_ERS_ErlangIO_HXX
#include "ErlangMsgCodec.hxx"
#include <iostream>
#include <vector>

/**
 *  Handles all the input/output from/to the erlang.
 *  Other side of the communication protocol is implemented in bio_ers_solver.erl.
 */
class ErlangIO {
private:
    int packetSize;
    std::istream &in;
    std::ostream &out;
    std::ostream *log;

    int bufSize;
    char *buf;

    std::vector<ErlangMsgCodec*> codecs;

public:
    /**
     *  Constructor.
     *  Stores specified streams and makes them unbuffered, initializes other things.
     *  @param inStream Stream for receiving messages from Erlang.
     *  @param outStream Stream for sending messages to Erlang.
     *  @param packetSize used by the erlang port protocol (specified when opening a port),
     *      means field size in bytes, which stores message lenght in bytes. By default 2
     *      should be used on both ends.
     */
    ErlangIO(std::istream &inStream, std::ostream &outStream, int packetSize = 2);

    /**
     *  Destructor.
     */
    ~ErlangIO();

    /**
     *  Sets stream for writing logs. The logging is disabled if 0 (null) is passed here.
     *  @param log Log stream or null, if logging should be disabled.
     *  @returns Previous logging stream or null if logging was disabled.
     */
    std::ostream* setLog(std::ostream* logStream);

    /**
     *  Add Erlang message encoders/decoders.
     *  @param codec Encoder/decoder for the Erlang messages.
     */
    void addMessageCodec(ErlangMsgCodec* codec);

    /**
     *  Tells, if communication with erlang is still possible.
     */
    bool live();

    /**
     * XXX: Temporary test.
     */
    void test();

protected:
    int readMessage(char* buf, int bufLen);
    int readBytes(char* buf, int count);
};


#endif
