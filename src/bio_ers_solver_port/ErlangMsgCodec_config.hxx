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
#ifndef BIO_ERS_ErlangMsgCodec_config_HXX
#define BIO_ERS_ErlangMsgCodec_config_HXX
#include "ErlangMsgCodec.hxx"
#include <string>
#include <map>
#include <ei.h>

/**
 *  Codec for the 'config' command.
 *  The message is a tuple in the follwoing form:
 *  \code
 *      {config, self(), {solver_state_v1,
 *          State,
 *          Model,
 *          DataDir,
 *          Params,
 *          Concentrations,
 *          Response
 *      }}
 *  \endcode
 */
class ErlangMsgCodec_config : public ErlangMsgCodec
{
private:
    static std::string TUPLE_NAME;
    static int         TUPLE_ARITY;

    static std::string SOLVER_STATE_NAME;
    static int         SOLVER_STATE_ARITY;

    static std::string PARAM_NAME;
    static int         PARAM_ARITY;

    erlang_pid pid;
    std::string model;
    std::map<std::string, double> params;
    std::string concentrations;

public:
    /**
     *  Constructor.
     */
    ErlangMsgCodec_config();

    /**
     *  Destructor.
     */
    virtual ~ErlangMsgCodec_config();

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

