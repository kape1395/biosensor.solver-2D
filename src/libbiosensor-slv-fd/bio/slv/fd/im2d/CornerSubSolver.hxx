#ifndef BIO_SLV_FD_IM2D_CornerSubSolver_HXX
#define BIO_SLV_FD_IM2D_CornerSubSolver_HXX
#include "../../../../biosensor-slv-fd.hxx"
#include "../FiniteDifferencesSolverAnalyzer.hxx"
#include <bio/cfg/StructureAnalyzer.hxx>
#include <log4cxx/logger.h>

BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *
 */
class CornerSubSolver
{
private:
    log4cxx::LoggerPtr log;

public:

    /**
     *
     */
    CornerSubSolver(
        int positionH,
        int positionV,
        BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
        BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer* fdAnalyzer
    );

    /**
     *
     */
    virtual ~CornerSubSolver();

    /**
     *  Solve...
     */
    void solveForward();

    /**
     *  Solve...
     */
    void solveBackward();

};


BIO_SLV_FD_IM2D_NS_END

#endif
