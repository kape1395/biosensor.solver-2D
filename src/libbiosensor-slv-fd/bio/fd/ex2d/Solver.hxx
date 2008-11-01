#ifndef BIO_SLV_FD_EX2D_Solver_HXX
#define BIO_SLV_FD_EX2D_Solver_HXX
#include <bio/slv/AbstractIterativeSolver.hxx>

BIO_SLV_FD_EX2D_NS_BEGIN


/**
 *  
 */
class Solver : public bio::slv::AbstractIterativeSolver
{
public:
    Solver(BIO_XML_NS::model::Model* config) : AbstractIterativeSolver(config) {}
    
protected:
    virtual void solveIteration() {}
};



BIO_SLV_FD_EX2D_NS_END
        
#endif
