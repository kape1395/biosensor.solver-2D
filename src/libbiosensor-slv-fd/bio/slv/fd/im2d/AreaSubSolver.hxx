#ifndef BIO_SLV_FD_IM2D_AreaSubSolver_HXX
#define BIO_SLV_FD_IM2D_AreaSubSolver_HXX
#include "../../../../biosensor-slv-fd.hxx"

BIO_SLV_FD_IM2D_NS_BEGIN
class AreaSubSolver;
BIO_SLV_FD_IM2D_NS_END

#include "Solver.hxx"
#include "../FiniteDifferencesSolverAnalyzer.hxx"
#include <bio/cfg/StructureAnalyzer.hxx>
#include <log4cxx/logger.h>

BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *  This solver is responsible for one homogenous rectangular area.
 */
class AreaSubSolver
{
public:
    class EdgeData;

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


    //! Allocated edge data objects.
    std::vector<EdgeData*> edges;

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

    /**
     *  Get object that provides access to the data
     *  at the specified edge of the area.
     *
     *  \param substance    Global substance index.
     *  \param horizontal   Is bound horizontal (true) or vertical (false)?
     *  \param start        is that bound at top|left (true) or bottom|right (false).s
     *  \return Reference to the edge data.
     */
    EdgeData* getEdgeData(
        int substance,
        bool horizontal,
        bool start
    );

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

public:

    /**
     *  This class is used by bound subsolvers to access the concentrations
     *  and P-Q constants on the edge of this structure.
     */
    class EdgeData
    {
        friend class AreaSubSolver;

    private:
        AreaSubSolver* solver;
        int size;
        double stepSize;    //!< distande from #data0 to #data1
        double **data0;     //!< data0[x][layer]
        double **data1;     //!< data1[x][layer]

    protected:

        /**
         *
         */
        EdgeData(
            AreaSubSolver* solver,
            int substance,
            bool horizontal,
            bool start
        );

    public:

        ~EdgeData();

        int getSize()
        {
            return size;
        }

        double getStepSize()
        {
            return stepSize;
        }

        void setP0(int index, double value)
        {
            data0[index][LAYER_P] = value;
        }

        double getP0(int index)
        {
            return data0[index][LAYER_P];
        }

        double getP1(int index)
        {
            return data1[index][LAYER_P];
        }

        void setQ0(int index, double value)
        {
            data0[index][LAYER_Q] = value;
        }

        double getQ0(int index)
        {
            return data0[index][LAYER_Q];
        }

        double getQ1(int index)
        {
            return data1[index][LAYER_Q];
        }

        void setC0(int index, double value)
        {
            data0[index][solver->getCurrentLayerIndex()] = value;
        }

        double getC0(int index)
        {
            return data0[index][solver->getCurrentLayerIndex()];
        }

        double getC1(int index)
        {
            return data1[index][solver->getCurrentLayerIndex()];
        }

    };

};



BIO_SLV_FD_IM2D_NS_END

#endif
