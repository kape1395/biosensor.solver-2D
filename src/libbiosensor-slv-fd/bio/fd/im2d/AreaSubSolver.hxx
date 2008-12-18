#ifndef BIO_SLV_FD_IM2D_AreaSubSolver_HXX
#define BIO_SLV_FD_IM2D_AreaSubSolver_HXX
#include "../../../biosensor-slv-fd.hxx"

BIO_SLV_FD_IM2D_NS_BEGIN
class AreaSubSolver;
BIO_SLV_FD_IM2D_NS_END

#include "../FiniteDifferencesSolverAnalyzer.hxx"
#include "Solver.hxx"
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

    Solver *solver;
    int positionH;
    int positionV;

    static const int LAYER_INTERM = 1;
    static const int LAYER_P = 3;
    static const int LAYER_Q = 4;
    static const int LAYER_f_R = 5;
    double ****data;    //  [h][v][s][(curr|prev)Layer, intermLayer, (curr|this)Layer, p, q, f_R]
    int dataSizeH;      //  number of points in H (including boundary)
    int dataSizeV;      //  number of points in V (including boundary)
    int dataSizeS;      //  number of modelled substances
    std::vector<int> substanceIndexes;  //  Indexes of the modelled substances. [0..dataSizeS-1]->substanceIndexInTheModel

    double stepSizeH;   // Step size by H direction.
    double stepSizeV;   // Step size by V direction.
    double startPositionH;  // position in coord. system, where this ub-area starts (H).
    double startPositionV;  // position in coord. system, where this ub-area starts (V).

    bool coordinateSystemIsCartesian;
    bool coordinateSystemIsCylindrical;

    /**
     *  Indicates, are layers inverted or not.
     *  \see getCurrentLayerIndex() getPreviousLayerIndex()
     */
    bool dataLayersInverted;

    /**
     *  Diffusion coefficients for all needed substances, array size is #dataSizeS.
     *  Here 0.0 means no diffusion.
     */
    double *D;

    ////////////////////////////////////////
    // reaction related things
    struct ReactionMMPart
    {
        int substrateIndex;
        double V_max;   // with + or -, depending on the substance to apply.
        double K_M;
    };
    ReactionMMPart **reactionsMM;   // reactionsMM[substance][MM_part];
    int *reactionsMMPartCounts;     // reactionsMMPartCounts[substance];

    struct ReactionROPart
    {
        int substrate1Index;
        int substrate2Index;
        double rate;    // with + or -, depending on the substance to apply.
    };
    ReactionROPart **reactionsRO;   // reactionsRO[substance][MM_part];
    int *reactionsROPartCounts;     // reactionsROPartCounts[substance];
    // reaction related things
    ////////////////////////////////////////


public:

    /**
     *  Constructor.
     */
    AreaSubSolver(
        Solver* solver,
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
    void solveHorizontalForward();

    /**
     *  First half-step -- horizontal.
     */
    void solveHorizontalBackward();

    /**
     *  Second half-step -- vertical.
     */
    void solveVerticalForward();

    /**
     *  Second half-step -- vertical.
     */
    void solveVerticalBackward();

protected:

    /**
     *  Converts global substance index to local.
     *
     *  \param globalSubstanceIndex Substance index, as defined in the model.
     *  \return local substance index or -1 if there
     *          is no corresponding local substance.
     */
    int getLocalSubstanceIndex(int globalSubstanceIndex);

    /**
     *  Returns an index of the current layer (in the time dimension) in the structure "data".
     *  Between calculations the "current" layer has a result of the last iteration.
     *  When calculations are running, current layer means the destination
     *  of the calculations.
     *
     *  \return Index of "current".
     */
    int getCurrentLayerIndex()
    {
        return dataLayersInverted ? 2 : 0;
    }

    /**
     *  Returns an index of the previous layer (in the time dimension) in the structure "data".
     *  Between calculations the "previous" layer has a result the previous iteration.
     *  When calculations are running, previous layer means the source (basis)
     *  of the calculations.
     *
     *  \return Index of "current".
     */
    int getPreviousLayerIndex()
    {
        return dataLayersInverted ? 0 : 2;
    }
};



BIO_SLV_FD_IM2D_NS_END

#endif
