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
#include "ErlangMsgCodec_conf.hxx"
#include "ErlangRecordDef.hxx"
#define LOG(message) if (log) (*log) << "ErlangMsgCodec_conf: " << message << std::endl


/* ************************************************************************** */
/* ************************************************************************** */
ErlangMsgCodec_conf::ErlangMsgCodec_conf() : ErlangMsgCodec()
{
    model.clear();
    //concentrations.clear();
}


/* ************************************************************************** */
/* ************************************************************************** */
ErlangMsgCodec_conf::~ErlangMsgCodec_conf()
{
    cleanup();
}


/* ************************************************************************** */
/* ************************************************************************** */
bool ErlangMsgCodec_conf::encode()
{
    return false;
}


/* ************************************************************************** */
/* ************************************************************************** */
bool ErlangMsgCodec_conf::decode(char *msgBuf, int msgLen)
{
    int termIndex = 0;
    int termType;
    int termSize;
    char *charBuf;

    if (!isRecord(msgBuf, &termIndex, ErlangRecordDef::CONF_PORT))
        return false;

    // #1: Extract process PID.
    assertRC(ei_get_type(msgBuf, &termIndex, &termType, &termSize));
    assertType(termType, ERL_PID_EXT);
    assertRC(ei_decode_pid(msgBuf, &termIndex, &pid));

    // #2: Extract simulation ID.
    assertRC(ei_get_type(msgBuf, &termIndex, &termType, &termSize));
    assertType(termType, ERL_STRING_EXT);
    charBuf = new char[termSize + 1];
    assertRC(ei_decode_string(msgBuf, &termIndex, charBuf));
    id.assign(charBuf);
    delete [] charBuf;

    // #3: Extract model (XML in binary).
    if (!isRecord(msgBuf, &termIndex, ErlangRecordDef::MODEL))
        throw -4;

    // #3.1: Skip model type. TODO: Check model type.
    assertRC(ei_skip_term(msgBuf, &termIndex));

    // #3.2: Extraxt model.
    decodeBinaryToString(msgBuf, &termIndex, &model);

    // #4: Extract params
    assertRC(ei_get_type(msgBuf, &termIndex, &termType, &termSize));
    assertType(termType, ERL_LIST_EXT);
    assertRC(ei_decode_list_header(msgBuf, &termIndex, &termSize));
    for (int i = 0; i < termSize; i++)
        params.insert(params.end(), decodeParam(msgBuf, &termIndex));

    // #5: Extract concentrations
    //decodeBinaryToString(msgBuf, &termIndex, &concentrations);
    assertRC(ei_skip_term(msgBuf, &termIndex));

    LOG("Successfully decoded.");
    return true;
}

std::pair<std::string, double> ErlangMsgCodec_conf::decodeParam(char *msgBuf, int *termIndex)
{
    int termType;
    int termSize;

    if (!isRecord(msgBuf, termIndex, ErlangRecordDef::PARAM))
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
void ErlangMsgCodec_conf::cleanup()
{
    model.clear();
    params.clear();
    //concentrations.clear();
}


/* ************************************************************************** */
/* ************************************************************************** */
std::string& ErlangMsgCodec_conf::getModel()
{
    return model;
}


/* ************************************************************************** */
/* ************************************************************************** */
std::map<std::string, double>& ErlangMsgCodec_conf::getParams()
{
    return params;
}


/* ************************************************************************** */
/* ************************************************************************** */
