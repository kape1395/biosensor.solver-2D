#ifndef BIO_SLV_FD_EX2D_Solver_HXX
#define BIO_SLV_FD_EX2D_Solver_HXX
#include "../../../../biosensor-slv-fd.hxx"
#include <bio/slv/AbstractIterativeSolver.hxx>

BIO_SLV_FD_EX2D_NS_BEGIN


/**
 *
 */
class Solver : public BIO_SLV_NS::AbstractIterativeSolver
{
public:
    Solver(BIO_XML_NS::model::Model* config) : AbstractIterativeSolver(config) {}

    virtual ~Solver()
    {
        //  Empty virtual destructor.
    }

    /**
     *  Returns data-model.
     */
    virtual BIO_DM_NS::IDataModel* getData()
    {
        return 0;   // FIXME: Implement
    }

    /**
     *  Returns transducer.
     */
    virtual ITransducer* getTransducer()
    {
        return 0;   // FIXME: Implement
    }

protected:
    virtual void solveIteration() {}
};



BIO_SLV_FD_EX2D_NS_END

#endif
