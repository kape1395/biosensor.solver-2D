#include "FilesystemOutputContext.hxx"
#include <sstream>
#include <iomanip>
#include <fstream>
#include <string>
#include <boost/filesystem.hpp>
#include "../Exception.hxx"

namespace bf = boost::filesystem;


/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::FilesystemOutputContext::FilesystemOutputContext(
    const std::string& baseDir
) : baseDirPath(baseDir)
{
    if (bf::exists(baseDirPath) && bf::is_directory(baseDirPath))
    {
        throw BIO_NS::Exception("Target directory alredy exists.");
    }
    if (!bf::create_directory(baseDirPath))
    {
        throw BIO_NS::Exception("Unable to create directory base directory");
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::FilesystemOutputContext::~FilesystemOutputContext()
{
    close();
}


/* ************************************************************************** */
/* ************************************************************************** */
std::ostream* BIO_IO_NS::FilesystemOutputContext::getOutputStream(const std::string& name)
{
    for (std::vector<std::string>::iterator fn = fileNames.begin(); fn < fileNames.end(); fn++)
    {
        if (fn->compare(name) == 0)
            throw Exception("OutputStream with specified name is alredy used.");
    }

    std::ofstream* out = new std::ofstream();
    out->open(getFilePath(name).file_string().c_str(), std::ios_base::out);

    fileNames.push_back(name);
    openOStreams.insert(std::pair<std::string,std::ofstream*>(name, out));
    return out;
}


/* ************************************************************************** */
/* ************************************************************************** */
std::ostream* BIO_IO_NS::FilesystemOutputContext::getOutputStream(const std::string& name, long index)
{
    return getOutputStream(createIndexedFileName(name, index));
}


/* ************************************************************************** */
/* ************************************************************************** */
std::istream* BIO_IO_NS::FilesystemOutputContext::getInputStream(const std::string& name)
{
    // TODO: Implement.
    return 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
std::istream* BIO_IO_NS::FilesystemOutputContext::getInputStream(const std::string& name, long index)
{
    // TODO: Implement.
    return 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_IO_NS::FilesystemOutputContext::close(std::ostream* stream)
{
    for (std::map<std::string, std::ofstream*>::iterator i = openOStreams.begin(); i != openOStreams.end(); i++)
    {
        if (i->second == stream)
        {
            i->second->flush();
            i->second->close();
            delete i->second;
            openOStreams.erase(i);
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_IO_NS::FilesystemOutputContext::close()
{
    for (std::map<std::string, std::ofstream*>::iterator i = openOStreams.begin(); i != openOStreams.end(); i++)
    {
        i->second->flush();
        i->second->close();
        delete i->second;
    }
    openOStreams.clear();
}


/* ************************************************************************** */
/* ************************************************************************** */
bf::path BIO_IO_NS::FilesystemOutputContext::getFilePath(const std::string& name) const
{
    bf::path filePath(baseDirPath);
    filePath /= name;
    return filePath.string();
}


/* ************************************************************************** */
/* ************************************************************************** */
std::string BIO_IO_NS::FilesystemOutputContext::createIndexedFileName(const std::string& name, long index) const
{
    std::stringstream fileName;
    fileName << name << '-';
    fileName << std::setfill('0') << std::setw(10);
    fileName << index;
    return fileName.str();
}


/* ************************************************************************** */
/* ************************************************************************** */
