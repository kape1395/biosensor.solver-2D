#ifndef BIOSENSOR_SOLV_FD_HXX
#define BIOSENSOR_SOLV_FD_HXX

// TODO: Cia kazkaip NS nesusirisa su configure.h

#define BIO_NS bio
#define BIO_NS_BEGIN namespace BIO_NS {
#define BIO_NS_END }

#define BIO_SLV_FD_NS BIO_NS::slv::fd
#define BIO_SLV_FD_NS_BEGIN BIO_NS_BEGIN namespace slv { namespace fd {
#define BIO_SLV_FD_NS_END BIO_NS_END } }

#define BIO_SLV_FD_EX2D_NS BIO_SLV_FD_NS::ex2d
#define BIO_SLV_FD_EX2D_NS_BEGIN BIO_SLV_FD_NS_BEGIN namespace ex2d {
#define BIO_SLV_FD_EX2D_NS_END BIO_SLV_FD_NS_END }

#include "bio/fd/ex2d/Solver.hxx"


#endif
