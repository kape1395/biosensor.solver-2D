#ifndef BIO_SLV_NullSolver_HXX
#define BIO_SLV_NullSolver_HXX
#include "../../biosensor.hxx"
#include "AbstractSolver.hxx"
#include <biosensor-xml.hxx>
BIO_SLV_NS_BEGIN


/**
 *  Solver that doas nothing.
 */
class NullSolver : public AbstractSolver
{
public:
    NullSolver(BIO_XML_NS::model::Model* config);
    virtual ~NullSolver();
    virtual void solve();
};


BIO_SLV_NS_END
#endif
