#include <iostream>
#include <fstream>
#include <ei.h>
#include <erl_interface.h>


class ErlangIO {
private:
    int packetSize;
    std::istream &in;
    std::ostream &out;
    std::ostream &log;

public:
    ErlangIO(std::istream &inStream, std::ostream &outStream, std::ostream &logStream, int packetSize = 2);
    ~ErlangIO();
    int readMessage(char* buf, int bufLen);

protected:
    int readBytes(char* buf, int count);

};


/**
 *  Entry point for the bio_ers_solver_port.
 */
int main(int argn, char **argv)
{

    erl_init(0, 0);

    std::streamsize bufSize = 1024;
    char *buf = new char[bufSize];

    std::ofstream log;
    log.open("bio_ers_solver_port.log", std::fstream::out);

    ErlangIO eio(std::cin, std::cout, log, 2);
    while (std::cin.good())
    {
        int messageSize = eio.readMessage(buf, bufSize);
        if (messageSize > 0)
        {
            int termIndex = 0;
            int termType;
            int termSize;
            int eirc;
            eirc = ei_get_type(buf, &termIndex, &termType, &termSize);
            log << "MSG: eirc=" << eirc << " termIndex=" << termIndex
                << " msgSize=" << messageSize
                << " termType=" << termType
                << " termSize=" << termSize
                << std::endl;

            int binVersion = 0;
            eirc = ei_decode_version(buf, &termIndex, &binVersion);
            log << "  VERSI: eirc=" << eirc << " termIndex=" << termIndex << " version=" << binVersion << std::endl;

            int tupleArity;
            ei_decode_tuple_header(buf, &termIndex, &tupleArity);
            log << "  TUPLE: eirc=" << eirc << " arity=" << tupleArity << std::endl;

        }
    }

    log.close();
    delete [] buf;
    return 0;
}

/**
 *  Constructor.
 *  Stores specified streams and makes them unbuffered.
 */
ErlangIO::ErlangIO(std::istream &inStream, std::ostream &outStream, std::ostream &logStream, int packetSize) 
    : in(inStream), out(outStream), log(logStream)
{
    this->packetSize = packetSize;

    out.setf(std::ios::unitbuf);
    out.rdbuf()->pubsetbuf(0, 0);

    in.setf(std::ios::unitbuf);
    in.rdbuf()->pubsetbuf(0, 0);
}

/**
 *  Destructor.
 */
ErlangIO::~ErlangIO()
{
    // ok
}


int ErlangIO::readMessage(char* buf, int bufLen)
{
    char lenBuf[packetSize];
    if (readBytes(lenBuf, packetSize) != packetSize)
        return -1;

    int len = 0;
    for (int i = 0; i < packetSize; i++)
        len = (len << 8) | lenBuf[i];

    if (bufLen < len)
        return -1;
    else
        return readBytes(buf, len);
}


int ErlangIO::readBytes(char* buf, int count)
{
    int readCount = std::cin.read(buf, count).gcount();

    log << "READ:" 
       << " count=" << readCount << " good=" << in.good() << " bad=" << in.bad() 
       << " eof=" << in.eof() << " fail=" << in.fail() << std::endl;
    for (int i = 0; i < readCount; i++) 
        log << "DATA: " << std::hex << (int) buf[i] << std::dec << " '" << buf[i] << "'" << std::endl;

    if (count == readCount)
        return readCount;
    else
        return -1;
}


