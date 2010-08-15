#ifndef BIO_SLV_FD_IM1D_Solver_HXX
#define BIO_SLV_FD_IM1D_Solver_HXX
#include "../../../../biosensor-slv-fd.hxx"

BIO_SLV_FD_IM1D_NS_BEGIN
class Solver;
BIO_SLV_FD_IM1D_NS_END

#include "../im2d/Solver.hxx"
#include "../FiniteDifferencesSolverAnalyzer.hxx"

BIO_SLV_FD_IM1D_NS_BEGIN


/**
 *  Solver: one-dimensional, implemented using implicit scheme.
 */
class Solver : public BIO_SLV_FD_IM2D_NS::Solver
{

public:
    /**
     *  Constructor.
     */
    Solver(
        BIO_XML_NS::model::Model* config,
        BIO_NS::IFactory* factory
    );

    /**
     *  Destructor.
     */
    virtual ~Solver();

};



BIO_SLV_FD_IM1D_NS_END

#endif
