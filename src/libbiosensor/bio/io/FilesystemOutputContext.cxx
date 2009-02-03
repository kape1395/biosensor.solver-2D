#include "FilesystemOutputContext.hxx"
#include <sstream>
#include <iomanip>
#include <fstream>
#include <boost/filesystem.hpp>
#include "../Exception.hxx"

/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::FilesystemOutputContext::FilesystemOutputContext(std::string& baseDir)
{
    using namespace boost;
    this->baseDir = baseDir;
    
    filesystem::path baseDirPath(baseDir);
    if (filesystem::exists(baseDirPath) && filesystem::is_directory(baseDirPath))
    {
        throw Exception("target directory alredy exists.");
    }
    filesystem::create_directory(baseDirPath);
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::FilesystemOutputContext::~FilesystemOutputContext()
{
    // TODO: Implement.
}


/* ************************************************************************** */
/* ************************************************************************** */
std::ostream* BIO_IO_NS::FilesystemOutputContext::getOutputStream(std::string& name)
{
    // TODO: Implement.
    return 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
std::ostream* BIO_IO_NS::FilesystemOutputContext::getOutputStream(std::string& name, long index)
{
    using namespace boost;
    /*
    path path(getFileName(name, index));
    if (exists(path))
    {
        
    }
     */


    //std::string& fileName = getFileName(name, index);
    //std::ofstream* file = new std::ofstream(fileName);
    //file->
    // TODO: Implement.
    return 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
std::istream* BIO_IO_NS::FilesystemOutputContext::getInputStream(std::string& name)
{
    // TODO: Implement.
    return 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
std::istream* BIO_IO_NS::FilesystemOutputContext::getInputStream(std::string& name, long index)
{
    // TODO: Implement.
    return 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_IO_NS::FilesystemOutputContext::close(std::ostream* stream)
{
    // TODO: Implement.
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_IO_NS::FilesystemOutputContext::close()
{
    // TODO: Implement.
}


/* ************************************************************************** */
/* ************************************************************************** */
std::string BIO_IO_NS::FilesystemOutputContext::getFileName(std::string& name)
{
    std::stringstream fileName;
    fileName << baseDir << "/" << name;
    return fileName.str();
}
/* ************************************************************************** */
/* ************************************************************************** */
std::string BIO_IO_NS::FilesystemOutputContext::getFileName(std::string& name, long index)
{
    std::stringstream fileName;
    fileName << getFileName(name);
    fileName << std::setfill('0') << std::setw(10);
    fileName << index;
    return fileName.str();
}
/* ************************************************************************** */
/* ************************************************************************** */
