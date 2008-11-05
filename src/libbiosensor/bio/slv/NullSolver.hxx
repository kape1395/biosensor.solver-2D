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
    NullSolver(BIO_XML_NS::model::Model* config) : AbstractSolver(config)
    {
        data = new BIO_DM_NS::NullDM();
    }

    /**
     *  Destructor.
     */
    virtual ~NullSolver()
    {
        delete data;
    }

    /**
     *  Returns data-model.
     */
    virtual BIO_DM_NS::IDataModel* getData()
    {
        return data;
    }

    /**
     *  Solves nothing.
     */
    virtual void solve()
    {
        // Nothing to do here.
    }

};


BIO_SLV_NS_END
#endif
