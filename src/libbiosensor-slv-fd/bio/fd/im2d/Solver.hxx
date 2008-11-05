#ifndef BIO_SLV_FD_IM2D_Solver_HXX
#define BIO_SLV_FD_IM2D_Solver_HXX
#include "../../../biosensor-slv-fd.hxx"
#include "AreaSubSolver.hxx"
#include "BoundSubSolver.hxx"
#include "CornerSubSolver.hxx"
#include <bio/Splitted2DArea.hxx>
#include <bio/cfg/StructureAnalyzer.hxx>
#include <bio/slv/AbstractIterativeSolver.hxx>
#include <log4cxx/logger.h>

BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *  Solver: two-dimensional, implemented using implicit scheme.
 *  Method of alternating directions is used to reduce order of ...
 */
class Solver : public BIO_SLV_NS::AbstractIterativeSolver
{
private:
    typedef BIO_NS::Splitted2DArea<AreaSubSolver*, BoundSubSolver*, CornerSubSolver*> SplittedSolver;
    
    log4cxx::LoggerPtr log;
    BIO_CFG_NS::StructureAnalyzer structAnalyzer;
    SplittedSolver *subSolvers;
    
public:
    /**
     *  Constructor.
     */
    Solver(BIO_XML_NS::model::Model* config);
    
    /**
     *  Destructor.
     */
    virtual ~Solver();
    
    /**
     *  Returns data-model.
     */
    virtual BIO_DM_NS::IDataModel* getData()
    {
        return 0;   // FIXME: Implement
    }
    
protected:
    
    /**
     *  Implementation of AbstractIterativeSolver::solveIteration().
     *  This method solves one full iteration (step in time).
     */
    virtual void solveIteration();
    
    /**
     *  Prepares all internal structure of the solver.
     *  FIXME: Mayme move this to the constructor?
     */
    void constructSolver();
    
};



BIO_SLV_FD_IM2D_NS_END
        
#endif
