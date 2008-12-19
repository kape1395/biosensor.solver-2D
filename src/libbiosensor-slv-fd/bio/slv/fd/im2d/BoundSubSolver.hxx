#ifndef BIO_SLV_FD_IM2D_BoundSubSolver_HXX
#define BIO_SLV_FD_IM2D_BoundSubSolver_HXX
#include "../../../biosensor-slv-fd.hxx"

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
 *  Bound solver.
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

    
    class BoundCondition {
        virtual ~BoundCondition()
        {};
        virtual void solveThroughForward() = 0;
        virtual void solveThroughBackward() = 0;
        virtual void solveAlongForward() = 0;
        virtual void solveAlongBackward() = 0;
    };
    class ConstantBoundCondition : public BoundCondition {
        
    };
    class WallBoundCondition : public BoundCondition {

    };
    class MergeBoundCondition : public BoundCondition {

    };
    class NullBoundCondition : public BoundCondition {

    };
};


BIO_SLV_FD_IM2D_NS_END

#endif
