#ifndef BIO_SLV_FD_IM2D_AreaSubSolver_HXX
#define BIO_SLV_FD_IM2D_AreaSubSolver_HXX
#include "../../../biosensor-slv-fd.hxx"
#include <log4cxx/logger.h>

BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *
 */
class AreaSubSolver
{
private:
    log4cxx::LoggerPtr log;
    
public:
    AreaSubSolver() : log(log4cxx::Logger::getLogger("libbiosensor-slv-fd::im2d::AreaSubSolver"))
    {
        LOG4CXX_DEBUG(log, "AreaSubSolver()");
    }
    
};



BIO_SLV_FD_IM2D_NS_END
        
#endif
