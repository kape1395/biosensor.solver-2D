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
#include <vector>

BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *  Corners now are implemented by solving bounds for corner points too,
 *  when solving along the bound.
 */
class CornerSubSolver : public BIO_DM_NS::IConcentrations
{
private:
    std::vector< std::vector<BIO_DM_NS::ICursor1D*> > cursors;  //  cursors [globalSubstanceIndex][number]
    std::vector< BIO_DM_NS::ICursor1D* > cursorsPlain;          // just the same as cursors, but just a plain list.
    unsigned substCount;

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
     *  Set concentration...
     */
    void setConcentration(int s, double c);

};


BIO_SLV_FD_IM2D_NS_END

#endif
