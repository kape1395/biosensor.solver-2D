#ifndef BIO_SLV_FD_IM2D_AreaSubSolver_HXX
#define BIO_SLV_FD_IM2D_AreaSubSolver_HXX
#include "../../../../biosensor-slv-fd.hxx"

BIO_SLV_FD_IM2D_NS_BEGIN
class AreaSubSolver;
BIO_SLV_FD_IM2D_NS_END

#include "Solver.hxx"
#include "IAreaEdgeData.hxx"
#include "../FiniteDifferencesSolverAnalyzer.hxx"
#include <bio/cfg/StructureAnalyzer.hxx>
#include <bio/dm/IGrid2D.hxx>
#include <bio/dm/ICursor2D.hxx>
#include <bio/dm/IConcentrations.hxx>
#include <bio/dm/AbstractCursor2D.hxx>
#include <string>

BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *  This solver is responsible for one homogenous rectangular area.
 */
class AreaSubSolver : public BIO_DM_NS::IGrid2D
{
public:
    class EdgeData;
    class EdgeDataPrevLayer;

private:

    BIO_CFG_NS::StructureAnalyzer* structAnalyzer;
    BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer* fdAnalyzer;

    Solver *solver;
    int positionH;
    int positionV;

    BIO_DM_NS::ISegmentSplit* pointPositionsH;
    BIO_DM_NS::ISegmentSplit* pointPositionsV;

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
     *  Indicates target layer of the current half-step. This variable can have
     *  values {0, 1, 2}, i.e. {(curr|prev)Layer, intermLayer, (curr|this)Layer}.
     *  Values for this variable are set in the solveXXX methods.
     */
    int targetLayerIndex;

    /**
     *  Diffusion coefficients for all needed substances for the horizontal direction.
     *  An array size is #dataSizeS. Here 0.0 means no diffusion.
     */
    double *D_h;

    /**
     *  Diffusion coefficients for all needed substances for the vertical direction.
     *  \see D_h.
     */
    double *D_v;

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

    int getPositionH()
    {
        return positionH;
    }

    int getPositionV()
    {
        return positionV;
    }

    int getPointCountH()
    {
        return dataSizeH;
    }

    int getPointCountV()
    {
        return dataSizeV;
    }

    /**
     *  Get object that provides access to the data
     *  at the specified edge of the area.
     *
     *  \param substance    Global substance index.
     *  \param horizontal   Is bound horizontal (true) or vertical (false)?
     *  \param start        is that bound at top|left (true) or bottom|right (false).s
     *  \return Reference to the edge data.
     */
    IAreaEdgeData* getEdgeData(
        int substance,
        bool horizontal,
        bool start,
        bool targetLayer = true
    );

    /**
     *  Get current concentration of the substance s at point (h, v).
     *
     *  \param s    Substance index (global).
     *  \param h    Horizontal point index (local).
     *  \param v    Vertical point index (local).
     *  \return Concentration, or NaN if subscence is not defined in this area.
     */
    double getConcentration(
        int h,
        int v,
        int s
    );

    /**
     *  Set new concentration of the substance s at point (h, v).
     *
     *  \param s    Substance index (global).
     *  \param h    Horizontal point index (local).
     *  \param v    Vertical point index (local).
     *  \param c    New concentration.
     */
    void setConcentration(
        int h,
        int v,
        int s,
        double c
    );

    /* ********************************************************************** */
    /* *********    IGrid2D implementation - start                            */
    /**
     *  Returns number of substances, defined in the entrie model.
     */
    virtual int getSubstanceCount();

    /**
     *  Returns substance conf. if i^th substance exists in this sub-area.
     *  If not - 0 is returned.
     */
    virtual BIO_XML_NS::model::Substance* getSubstanceConf(int index);

    virtual BIO_DM_NS::ISegmentSplit* getPointPositionsH();

    virtual BIO_DM_NS::ISegmentSplit* getPointPositionsV();

    virtual BIO_DM_NS::ICursor2D* newGridCursor();

    /* *********    IGrid2D implementation - start                            */
    /* ********************************************************************** */


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

    /**
     *  \see #targetLayerIndex.
     */
    int getTargetLayerIndex()
    {
        return targetLayerIndex;
    }

    /**
     *  This method is used for debugging only.
     */
    void dumpData(std::ostream& out, bool verticalIsInner, std::string message);

public:

    /**
     *  This class is used by bound subsolvers to access the concentrations
     *  and P-Q constants on the edge of this structure.
     */
    class EdgeData : public IAreaEdgeData
    {
        friend class AreaSubSolver;

    protected:
        AreaSubSolver* solver;
        int size;
        double stepSize;    //!< distande from #data0 to #data1
        double **data0;     //!< data0[x][layer]
        double **data1;     //!< data1[x][layer]
        bool forward;       //!< bound is forward (at the start of area, top or left).

    protected:
        EdgeData(
            AreaSubSolver* solver,
            int substance,
            bool horizontal,
            bool start
        );

    public:

        virtual ~EdgeData();

        virtual int getSize();
        virtual double getStepSize();
        virtual bool isForward();

        virtual void setP0(int index, double value);
        virtual double getP0(int index);
        virtual double getP1(int index);
        virtual void setQ0(int index, double value);
        virtual double getQ0(int index);
        virtual double getQ1(int index);
        virtual void setC0(int index, double value);
        virtual double getC0(int index);
        virtual double getC1(int index);
    };

    /**
     *  This is the same as EdgeData, but for a previous layer.
     *  This implementation is readonly and supports only C0 and C1.
     */
    class EdgeDataPrevLayer : public EdgeData
    {
        friend class AreaSubSolver;

    protected:
        EdgeDataPrevLayer(
            AreaSubSolver* solver,
            int substance,
            bool horizontal,
            bool start
        );

    public:

        virtual ~EdgeDataPrevLayer();

        virtual void setP0(int index, double value);
        virtual double getP0(int index);
        virtual double getP1(int index);
        virtual void setQ0(int index, double value);
        virtual double getQ0(int index);
        virtual double getQ1(int index);
        virtual void setC0(int index, double value);
        virtual double getC0(int index);
        virtual double getC1(int index);
    };


private:
    /**
     *  Cursor...
     */
    class Cursor : public BIO_DM_NS::AbstractCursor2D, public BIO_DM_NS::IConcentrations
    {
    private:
        AreaSubSolver* subSolver;

    public:

        /**
         *  Constructor.
         */
        Cursor(AreaSubSolver* subSolver);

        /**
         *  Destructor.
         */
        virtual ~Cursor();

        /**
         *  Returns concentrations of the substances at the current position.
         */
        virtual BIO_DM_NS::IConcentrations* getConcentrations();

        /**
         *  Returns concentration of the substance in a current point.
         *  This is implementation of an interface IConcentrations.
         */
        virtual double getConcentration(int substanceNr);

        /**
         *  Sets new concentration for the substance with specified index.
         *  @param substanceNr      Substance index.
         *  @param concentration    New concentration for the substance.
         */
        virtual void setConcentration(int substanceNr, double concentration);
    };


};



BIO_SLV_FD_IM2D_NS_END

#endif
