#ifndef BIO_SLV_FD_IM2D_CornerSubSolver_HXX
#define BIO_SLV_FD_IM2D_CornerSubSolver_HXX
#include "../../../../biosensor-slv-fd.hxx"
#include "../FiniteDifferencesSolverAnalyzer.hxx"
#include <bio/cfg/StructureAnalyzer.hxx>
#include <bio/dm/IConcentrations.hxx>
#include <log4cxx/logger.h>

BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *
 */
class CornerSubSolver : public BIO_DM_NS::IConcentrations
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

    /**
     *  return concentration of substance in this corner.
     *
     *  \param s    Global subsatnce index.
     */
    double getConcentration(int s);

    /**
     *  Returns concentration of the substance with index <code>substanceNr</code>.
     */
    virtual double operator [] (int substanceNr)
    {
        return getConcentration(substanceNr);
    }

};


BIO_SLV_FD_IM2D_NS_END

#endif
