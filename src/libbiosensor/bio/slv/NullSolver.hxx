#ifndef BIO_SLV_NullSolver_HXX
#define BIO_SLV_NullSolver_HXX
#include "../../biosensor.hxx"
#include "../dm/NullDM.hxx"
#include "AbstractSolver.hxx"
#include <biosensor-xml.hxx>
BIO_SLV_NS_BEGIN


/**
 *  Solver that doas nothing.
 */
class NullSolver : public AbstractSolver
{
private:
    BIO_DM_NS::NullDM *data;

public:

    /**
     *  Constructor.
     */
    NullSolver(BIO_XML_NS::model::Model* config);

    /**
     *  Destructor.
     */
    virtual ~NullSolver();

    /**
     *  Returns data-model.
     */
    virtual BIO_DM_NS::IDataModel* getData();

    /**
     *  Solves nothing.
     */
    virtual void solve();

};


BIO_SLV_NS_END
#endif
