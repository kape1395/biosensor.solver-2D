#ifndef BIO_SLV_FD_SolverFactory_HXX
#define BIO_SLV_FD_SolverFactory_HXX
#include "../../../biosensor-slv-fd.hxx"
#include <bio/slv/ISolverFactory.hxx>
BIO_SLV_FD_NS_BEGIN


/**
 *  Created solvers, that relates to this module (libbiosensor-slf-fd).
 */
class SolverFactory : public ISolverFactory
{
public:
    virtual ~SolverFactory()
    {
        //  Empty virtual destructor.
    }

    ISolver* create(BIO_XML_NS::model::Model* model);

};

BIO_SLV_FD_NS_END
#endif
