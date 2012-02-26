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
#include "ErlangMsgCodec_stop.hxx"
#define LOG(message) if (log) (*log) << "ErlangMsgCodec_stop: " << message << std::endl

std::string ErlangMsgCodec_stop::TUPLE_NAME("stop");
int ErlangMsgCodec_stop::TUPLE_ARITY = 2;

/* ************************************************************************** */
/* ************************************************************************** */
ErlangMsgCodec_stop::ErlangMsgCodec_stop() : ErlangMsgCodec()
{
    // Nothing.
}


/* ************************************************************************** */
/* ************************************************************************** */
ErlangMsgCodec_stop::~ErlangMsgCodec_stop()
{
    cleanup();
}


/* ************************************************************************** */
/* ************************************************************************** */
bool ErlangMsgCodec_stop::encode()
{
    return false;
}


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Decodes messages like: {stop, self()}
 */
bool ErlangMsgCodec_stop::decode(char *msgBuf, int msgLen)
{
    int eirc;
    int tupleArity;
    int termIndex = 0;
    int termType;
    int termSize;
    char atomName[MAXATOMLEN+1];

    // Is tuple and arity is correct?
    eirc = ei_decode_tuple_header(msgBuf, &termIndex, &tupleArity);
    LOG("Tuple arity=" << tupleArity);
    if (!(eirc == 0 && tupleArity == TUPLE_ARITY))
        return false;

    // #1: First element is atom of correct length?
    eirc = ei_get_type(msgBuf, &termIndex, &termType, &termSize);
    LOG("Atom type=" << termType << " size=" << termSize);
    if (!(eirc == 0 && termType == ERL_ATOM_EXT && termSize == (int) TUPLE_NAME.length()))
        return false;

    // #1: Is atom name correct?
    eirc = ei_decode_atom(msgBuf, &termIndex, atomName);
    LOG("Atom name=" << atomName);
    if (!(eirc == 0 && TUPLE_NAME.compare(atomName) == 0))
        return false;

    // #2: Extract process PID.
    eirc = ei_decode_pid(msgBuf, &termIndex, &pid);
    if (eirc != 0)
        return false;

    LOG("Successfully decoded.");
    return true;
}


/* ************************************************************************** */
/* ************************************************************************** */
void ErlangMsgCodec_stop::cleanup()
{
    // Nothing.
}


/* ************************************************************************** */
/* ************************************************************************** */
