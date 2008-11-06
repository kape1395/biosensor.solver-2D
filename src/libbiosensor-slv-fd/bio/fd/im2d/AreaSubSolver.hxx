#ifndef BIO_SLV_FD_IM2D_AreaSubSolver_HXX
#define BIO_SLV_FD_IM2D_AreaSubSolver_HXX
#include "../../../biosensor-slv-fd.hxx"
#include "../FiniteDifferencesSolverAnalyzer.hxx"
#include <bio/cfg/StructureAnalyzer.hxx>
#include <log4cxx/logger.h>

BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *
 */
class AreaSubSolver
{
private:
    log4cxx::LoggerPtr log;
    int positionH;
    int positionV;
    double ****data;  //[h][v][s][thisLayer, prevLayer, p, q]
    int dataSizeH;  // number of points in H (including boundary)
    int dataSizeV;  // number of points in V (including boundary)
    int dataSizeS;  // number of substances

public:

    /**
     *  Constructor.
     */
    AreaSubSolver(
        int positionH,
        int positionV,
        BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
        BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer* fdAnalyzer
    );

    /**
     *  Destructor.
     */
    virtual ~AreaSubSolver();

    /**
     *  First half-step -- horizontal.
     */
    void solveHorizontal();

    /**
     *  Second half-step -- vertical.
     */
    void solveVertical();

};



BIO_SLV_FD_IM2D_NS_END

#endif
