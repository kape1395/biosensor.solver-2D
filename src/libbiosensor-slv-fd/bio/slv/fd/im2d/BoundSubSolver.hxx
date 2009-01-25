#ifndef BIO_SLV_FD_IM2D_BoundSubSolver_HXX
#define BIO_SLV_FD_IM2D_BoundSubSolver_HXX
#include "../../../../biosensor-slv-fd.hxx"

BIO_SLV_FD_IM2D_NS_BEGIN
class BoundSubSolver;
BIO_SLV_FD_IM2D_NS_END

#include "Solver.hxx"
#include "AreaSubSolver.hxx"
#include "../FiniteDifferencesSolverAnalyzer.hxx"
#include "IBoundCondition.hxx"
#include <bio/cfg/StructureAnalyzer.hxx>
#include <bio/cfg/BoundAnalyzer.hxx>
#include <log4cxx/logger.h>

BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *  Bound solver. Mainly responsible for construction of the bound solver.
 *  \see ConstantCondition, WallCondition and MergeCondition.
 *
 */
class BoundSubSolver    // : public IBoundCondition
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

    /**
     *  This array maps global substance indexes to a corresponding
     *  bound condition.
     */
    IBoundCondition* * substanceToBCMap;
    
    std::vector<IBoundCondition*> boundConditions;

    typedef std::vector<IBoundCondition*>::iterator BCIterator;

public:

    /**
     *  Constructor. All initialization is done here.
     */
    BoundSubSolver(
        Solver* solver,
        int positionH,
        int positionV,
        bool horizontal,
        AreaSubSolver* areaPrev,
        AreaSubSolver* areaNext,
        BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
        BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer* fdAnalyzer,
        BIO_CFG_NS::BoundAnalyzer* boundAnalyzer
    );

    /**
     *  Destructor.
     */
    virtual ~BoundSubSolver();

    /**
     *  \see IBoundCondition#solveThroughForward.
     */
    void solveThroughForward();

    /**
     *  \see IBoundCondition#solveThroughBackward.
     */
    void solveThroughBackward();

    /**
     *  \see IBoundCondition#solveAlongForward.
     */
    void solveAlongForward();

    /**
     *  \see IBoundCondition#solveAlongBackward.
     */
    void solveAlongBackward();

    /**
     *  \see IBoundCondition#applyInitialValues.
     */
    void applyInitialValues();

    /**
     *  Returns current concentration of a substance.
     *
     *  \param x    Point index (local).
     *  \param s    Substance index (global).
     */
    double getConcentration(int x, int s);

protected:

    /**
     *  Creates bound condition for particular specification of
     *  substance for bound.
     */
    void createBoundCondition(
        BIO_XML_NS::model::BoundSubstance * boundSubstance,
        AreaSubSolver* areaPrev,
        AreaSubSolver* areaNext,
        int substance,
        bool atStart
    );

};


BIO_SLV_FD_IM2D_NS_END

#endif
