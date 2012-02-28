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
#include "ErlangMsgCodec_config.hxx"
#define LOG(message) if (log) (*log) << "ErlangMsgCodec_config: " << message << std::endl

std::string ErlangMsgCodec_config::TUPLE_NAME("config");
int         ErlangMsgCodec_config::TUPLE_ARITY = 3;

std::string ErlangMsgCodec_config::SOLVER_STATE_NAME("solver_state_v1");
int         ErlangMsgCodec_config::SOLVER_STATE_ARITY = 7;

std::string ErlangMsgCodec_config::PARAM_NAME("param");
int         ErlangMsgCodec_config::PARAM_ARITY = 3;


/* ************************************************************************** */
/* ************************************************************************** */
ErlangMsgCodec_config::ErlangMsgCodec_config() : ErlangMsgCodec()
{
    model.clear();
    concentrations.clear();
}


/* ************************************************************************** */
/* ************************************************************************** */
ErlangMsgCodec_config::~ErlangMsgCodec_config()
{
    cleanup();
}


/* ************************************************************************** */
/* ************************************************************************** */
bool ErlangMsgCodec_config::encode()
{
    return false;
}


/* ************************************************************************** */
/* ************************************************************************** */
bool ErlangMsgCodec_config::decode(char *msgBuf, int msgLen)
{
    int termIndex = 0;
    int termType;
    int termSize;

    if (!isRecord(msgBuf, &termIndex, TUPLE_NAME, TUPLE_ARITY))
        return false;

    // #2: Extract process PID.
    assertRC(ei_decode_pid(msgBuf, &termIndex, &pid));

    // #3: Extract solver state.
    if (!isRecord(msgBuf, &termIndex, SOLVER_STATE_NAME, SOLVER_STATE_ARITY))
        throw -2;

    // #3.2: Skip state
    assertRC(ei_skip_term(msgBuf, &termIndex));

    // #3.3: Extract model (XML in binary).
    decodeBinaryToString(msgBuf, &termIndex, &model);

    // #3.4: Skip datadir
    assertRC(ei_skip_term(msgBuf, &termIndex));

    // #3.5: Extract params
    assertRC(ei_get_type(msgBuf, &termIndex, &termType, &termSize));
    assertType(termType, ERL_LIST_EXT);
    assertRC(ei_decode_list_header(msgBuf, &termIndex, &termSize));
    for (int i = 0; i < termSize; i++)
        params.insert(params.end(), decodeParam(msgBuf, &termIndex));

    // #3.6: Extract concentrations
    decodeBinaryToString(msgBuf, &termIndex, &concentrations);

    // #3.7: Skip response
    assertRC(ei_skip_term(msgBuf, &termIndex));

    LOG("Successfully decoded.");
    return true;
}

std::pair<std::string, double> ErlangMsgCodec_config::decodeParam(char *msgBuf, int *termIndex)
{
    int termType;
    int termSize;

    if (!isRecord(msgBuf, termIndex, PARAM_NAME, PARAM_ARITY))
        throw -5;

    std::string paramName;
    char paramNameBuf[MAXATOMLEN+1];
    assertRC(ei_get_type(msgBuf, termIndex, &termType, &termSize));
    assertType(termType, ERL_ATOM_EXT);
    assertRC(ei_decode_atom(msgBuf, termIndex, paramNameBuf));
    paramName.assign(paramNameBuf);

    double paramValue = 0;
    assertRC(ei_get_type(msgBuf, termIndex, &termType, &termSize));
    assertType(termType, ERL_FLOAT_EXT);
    assertRC(ei_decode_double(msgBuf, termIndex, &paramValue));

    LOG("Decoded #param{" << paramName << ", " << paramValue << "}");
    return std::pair<std::string, double>(paramName, paramValue);
}

/* ************************************************************************** */
/* ************************************************************************** */
void ErlangMsgCodec_config::cleanup()
{
    model.clear();
    params.clear();
    concentrations.clear();
}


/* ************************************************************************** */
/* ************************************************************************** */
std::string& ErlangMsgCodec_config::getModel()
{
    return model;
}


/* ************************************************************************** */
/* ************************************************************************** */
std::map<std::string, double>& ErlangMsgCodec_config::getParams()
{
    return params;
}


/* ************************************************************************** */
/* ************************************************************************** */
