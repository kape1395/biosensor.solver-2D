#ifndef BIO_SLV_FD_IM2D_CornerSubSolver_HXX
#define BIO_SLV_FD_IM2D_CornerSubSolver_HXX
#include "../../../../biosensor-slv-fd.hxx"

BIO_SLV_FD_IM2D_NS_BEGIN
class CornerSubSolver;
BIO_SLV_FD_IM2D_NS_END

#include "../FiniteDifferencesSolverAnalyzer.hxx"
#include "BoundSubSolver.hxx"
#include <bio/cfg/StructureAnalyzer.hxx>
#include <bio/dm/ICursor1D.hxx>
#include <bio/dm/IConcentrations.hxx>

BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *  Corners now are implemented by solving bounds for corner points too,
 *  when solving along the bound.
 */
class CornerSubSolver : public BIO_DM_NS::IConcentrations
{
private:
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer;
    BIO_DM_NS::ICursor1D** cursors;     //  Array of cursors [globalSubstanceIndex]

public:

    /**
     *
     */
    CornerSubSolver(
        Solver* solver,
        int positionH,
        int positionV,
        BoundSubSolver* boundTop,
        BoundSubSolver* boundRight,
        BoundSubSolver* boundBottom,
        BoundSubSolver* boundLeft,
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
