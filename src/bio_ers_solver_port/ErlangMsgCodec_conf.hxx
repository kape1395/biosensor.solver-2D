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
#ifndef BIO_ERS_ErlangMsgCodec_conf_HXX
#define BIO_ERS_ErlangMsgCodec_conf_HXX
#include "ErlangMsgCodec.hxx"
#include <string>
#include <map>
#include <ei.h>

/**
 *  Codec for the 'conf' command.
 *  The message is the record \code #configure_port{} \endcode .
 */
class ErlangMsgCodec_conf : public ErlangMsgCodec
{
private:
    erlang_pid pid;
    std::string id;
    std::string model;
    std::map<std::string, double> params;
    //std::string concentrations;

public:
    /**
     *  Constructor.
     */
    ErlangMsgCodec_conf();

    /**
     *  Destructor.
     */
    virtual ~ErlangMsgCodec_conf();

    /**
     *  @copydoc ErlangMsgCodec::encode()
     *  Encoding is not required and therefore is not implemented.
     */
    virtual bool encode();

    /**
     *  @copydoc ErlangMsgCodec::decode()
     */
    virtual bool decode(char *msgBuf, int msgLen);

    /**
     *  @copydoc ErlangMsgCodec::cleanup()
     */
    virtual void cleanup();

    /**
     *  Returns biosensor model as a string.
     *  The content is decoded from the model property of the solver state record.
     */
    std::string& getModel();

    /**
     *  Returns biosensor model parameters (overrides).
     *  The content is decoded from the params property in the solver state record.
     */
    std::map<std::string, double>& getParams();

protected:

    /**
     *  Decodes #param record.
     *  @param msgBuf EI buffer.
     *  @param termIndex position in msgBuf from where to start decoding.
     *  @return map pair, describing one parameter (name -> value).
     */
    std::pair<std::string, double> decodeParam(char *msgBuf, int *termIndex);

};

#endif

