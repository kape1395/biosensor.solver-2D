#ifndef BIOSENSOR_XML_HXX
#define BIOSENSOR_XML_HXX


// TODO: Cia kazkaip NS nesusirisa su configure.h


#define BIO_NS bio
#define BIO_NS_BEGIN namespace BIO_NS {
#define BIO_NS_END }

#define BIO_XML_NS BIO_NS::xml
#define BIO_XML_NS_BEGIN BIO_NS_BEGIN namespace xml {
#define BIO_XML_NS_END BIO_NS_END }


#include "ModelBoundCondition.hxx"
#include "ModelMediumReaction.hxx"
#include "Model.hxx"
#include "ModelSolver.hxx"

#endif