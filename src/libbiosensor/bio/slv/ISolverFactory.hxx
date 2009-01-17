#ifndef BIO_SLV_ISolverFactory_HXX
#define BIO_SLV_ISolverFactory_HXX
#include "../../biosensor.hxx"
#include "ISolver.hxx"
#include <biosensor-xml.hxx>

BIO_SLV_NS_BEGIN


/**
 *  Interface, that all solver factories must implement.
 *  All modules, that supplies solvers supplies also
 *  implementations of this interface.
 *
 *  Apart from module-related factories there can be some generic factories,
 *  that can also implement discovery of needed factories and delegate creation
 *  of solvers to them.
 */
class ISolverFactory
{
public:

    virtual ~ISolverFactory()
    {
        //  Empty virtual destructor.
    }

    /**
     *  Create a factory from a model.
     *
     *  \returns
     *      Created solver or 0 - if does not know the solver,
     *      specified in the model.
     */
    virtual ISolver* create(BIO_XML_NS::model::Model* model) = 0;

};

BIO_SLV_NS_END

#endif
