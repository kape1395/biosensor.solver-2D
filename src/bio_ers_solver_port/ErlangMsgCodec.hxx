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

/**
 *  Base class for all Erlang message encoders/decoders.
 */
class ErlangMsgCodec
{
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

protected:

    /**
     *  Throws Exception if eirc indicates an error.
     *  @param eirc return code from functions in the Erlang EI library.
     */
    void assertrc(int eirc);
};

#endif
