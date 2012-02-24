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
#include "ErlangIO.hxx"
#include <iostream>
#include <fstream>
#include <ei.h>
#include <erl_interface.h>

/* ************************************************************************** */
/* ************************************************************************** */
ErlangIO::ErlangIO(std::istream &inStream, std::ostream &outStream, int packetSize)
    : in(inStream), out(outStream)
{
    this->packetSize = packetSize;

    out.setf(std::ios::unitbuf);
    out.rdbuf()->pubsetbuf(0, 0);

    in.setf(std::ios::unitbuf);
    in.rdbuf()->pubsetbuf(0, 0);

    setLog(0);

    erl_init(0, 0);

    bufSize = 1024;
    buf = new char[bufSize];

    currentMsg = 0;
}

/* ************************************************************************** */
/* ************************************************************************** */
ErlangIO::~ErlangIO()
{
    if (buf != 0)
        delete [] buf;
    codecs.clear();
}


/* ************************************************************************** */
/* ************************************************************************** */
std::ostream* ErlangIO::setLog(std::ostream* logStream)
{
    std::ostream* prev = log;
    log = logStream;
    return prev;
}


/* ************************************************************************** */
/* ************************************************************************** */
void ErlangIO::addMessageCodec(ErlangMsgCodec* codec)
{
    codecs.push_back(codec);
}


/* ************************************************************************** */
/* ************************************************************************** */
bool ErlangIO::live()
{
    return in.good();
}


/* ************************************************************************** */
/* ************************************************************************** */
ErlangMsgCodec* ErlangIO::getMessage(bool blocking)
{
    if (currentMsg) // Current MSG is not consumed yet.
        return currentMsg;

    int messageSize = readMessage(blocking);
    if (messageSize == 0)
    {
        return 0; // No message is currently available (non-blocking mode).
    }
    if (messageSize < 0)
    {
        if (log)
            (*log) << "Message cannot be parsed.";
        return 0; // Error.
    }

    // TODO: Here the message should be parsed.
    return 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
void ErlangIO::messageProcessed(ErlangMsgCodec* message)
{
    if (currentMsg == message)
        currentMsg = 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
/*
void ErlangIO::test()
{
    int messageSize = readMessage(buf, bufSize);
    if (messageSize > 0)
    {
        int termIndex = 0;
        int termType;
        int termSize;
        int eirc;
        eirc = ei_get_type(buf, &termIndex, &termType, &termSize);
        (*log) << "MSG: eirc=" << eirc << " termIndex=" << termIndex
               << " msgSize=" << messageSize
               << " termType=" << termType
               << " termSize=" << termSize
               << std::endl;

        int binVersion = 0;
        eirc = ei_decode_version(buf, &termIndex, &binVersion);
        (*log) << "  VERSI: eirc=" << eirc << " termIndex=" << termIndex << " version=" << binVersion << std::endl;

        int tupleArity;
        ei_decode_tuple_header(buf, &termIndex, &tupleArity);
        (*log) << "  TUPLE: eirc=" << eirc << " arity=" << tupleArity << std::endl;
    }
}
*/

/* ************************************************************************** */
/* ************************************************************************** */
int ErlangIO::readMessage(bool blocking)
{
    if (!blocking && in.rdbuf()->in_avail() == 0)
        return 0;

    //  Get message length
    int msgLen = 0;
    if (readBytes(buf, packetSize) != packetSize)
        return -1;
    for (int i = 0; i < packetSize; i++)
        msgLen = (msgLen << 8) | buf[i];

    //  Reallocate buffer if it is too small.
    if (bufSize < msgLen)
    {
        delete [] buf;
        bufSize = msgLen;
        buf = new char[bufSize];
        if (buf == 0)
            return -1;
    }

    //  Read it.
    return readBytes(buf, msgLen);
}


/* ************************************************************************** */
/* ************************************************************************** */
int ErlangIO::readBytes(char* readBuf, int readCount)
{
    int count = in.read(readBuf, readCount).gcount();

    if (log)
    {
        (*log) << "READ:"
               << " count=" << count
               << " good=" << in.good()
               << " bad=" << in.bad()
               << " eof=" << in.eof()
               << " fail=" << in.fail()
               << std::endl;
        for (int i = 0; i < count; i++)
            (*log) << "DATA: "
                   << std::hex << (int) readBuf[i] << std::dec
                   << " '" << readBuf[i] << "'" << std::endl;
    }
    if (count == readCount)
        return count;
    else
        return -1;
}


/* ************************************************************************** */
/* ************************************************************************** */
