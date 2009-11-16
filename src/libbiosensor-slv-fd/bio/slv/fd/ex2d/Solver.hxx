#ifndef BIO_SLV_FD_EX2D_Solver_HXX
#define BIO_SLV_FD_EX2D_Solver_HXX
#include "../../../../biosensor-slv-fd.hxx"
#include <bio/slv/AbstractIterativeSolver.hxx>

BIO_SLV_FD_EX2D_NS_BEGIN


/**
 *  Solver implementing 2D explicit schema for the method of finite differences.
 */
class Solver : public BIO_SLV_NS::AbstractIterativeSolver
{
public:
    /**
     *  Consructor
     *
     *  @param config Model to be solved.
     */
    Solver(BIO_XML_NS::model::Model* config);

    /**
     *  Destructor..
     */
    virtual ~Solver();

    /**
     *  Returns data-model.
     */
    virtual BIO_DM_NS::IDataModel* getData();

    /**
     *  Returns transducer.
     */
    virtual ITransducer* getTransducer();

    /**
     *  Set state for the solver.
     */
    virtual void setState(BIO_SLV_NS::ISolverState* state);


protected:
    virtual void solveIteration();

};



BIO_SLV_FD_EX2D_NS_END

#endif
