#ifndef BIO_SLV_FD_IM2D_BoundSubSolver_HXX
#define BIO_SLV_FD_IM2D_BoundSubSolver_HXX
#include "../../../../biosensor-slv-fd.hxx"

BIO_SLV_FD_IM2D_NS_BEGIN
class BoundSubSolver;
BIO_SLV_FD_IM2D_NS_END

#include "Solver.hxx"
#include "../FiniteDifferencesSolverAnalyzer.hxx"
#include <bio/cfg/StructureAnalyzer.hxx>
#include <bio/cfg/BoundAnalyzer.hxx>
#include <log4cxx/logger.h>

BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *  Bound solver. Mainly responsible for construction of the bound solver.
 *  \see ConstantCondition, WallCondition, MergeCondition, NullCondition.
 *
 *  XXX: Should it implement IBoundCondition?
 */
class BoundSubSolver
{
private:
    log4cxx::LoggerPtr log;

    Solver* solver;
    int positionH;
    int positionV;
    bool horizontal;
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer;
    BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer* fdAnalyzer;
    BIO_CFG_NS::BoundAnalyzer* boundAnalyzer;

public:

    /**
     *  Constructor. All initialization is done here.
     */
    BoundSubSolver(
        Solver* solver,
        int positionH,
        int positionV,
        bool horizontal,
        BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
        BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer* fdAnalyzer,
        BIO_CFG_NS::BoundAnalyzer* boundAnalyzer
    );

    /**
     *  Destructor.
     */
    virtual ~BoundSubSolver();

    /**
     *
     */
    void solveThroughForward();

    /**
     *
     */
    void solveThroughBackward();

    /**
     *
     */
    void solveAlongForward();

    /**
     *
     */
    void solveAlongBackward();

};


BIO_SLV_FD_IM2D_NS_END

#endif
