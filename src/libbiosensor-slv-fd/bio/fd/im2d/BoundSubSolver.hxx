#ifndef BIO_SLV_FD_IM2D_BoundSubSolver_HXX
#define BIO_SLV_FD_IM2D_BoundSubSolver_HXX
#include "../../../biosensor-slv-fd.hxx"
#include "../FiniteDifferencesSolverAnalyzer.hxx"
#include <bio/cfg/StructureAnalyzer.hxx>
#include <log4cxx/logger.h>

BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *
 */
class BoundSubSolver
{
private:
    log4cxx::LoggerPtr log;

public:

    /**
     *
     */
    BoundSubSolver(
        int positionH,
        int positionV,
        bool horizontal,
        BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
        BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer* fdAnalyzer
    );

    /**
     *
     */
    virtual ~BoundSubSolver();

    /**
     *
     */
    void solveThrough();

    /**
     *
     */
    void solveAlong();
    
};


BIO_SLV_FD_IM2D_NS_END

#endif
