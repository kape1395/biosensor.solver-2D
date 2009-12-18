#ifndef BIO_SLV_FD_IM2D_BoundSubSolver_HXX
#define BIO_SLV_FD_IM2D_BoundSubSolver_HXX
#include "../../../../biosensor-slv-fd.hxx"

BIO_SLV_FD_IM2D_NS_BEGIN
class BoundSubSolver;
BIO_SLV_FD_IM2D_NS_END

#include "Solver.hxx"
#include "AreaSubSolver.hxx"
#include "IAreaEdgeData.hxx"
#include "IAreaEdgeFunction.hxx"
#include "IBoundCondition.hxx"
#include "../FiniteDifferencesSolverAnalyzer.hxx"
#include <bio/cfg/StructureAnalyzer.hxx>
#include <bio/cfg/BoundAnalyzer.hxx>
#include <bio/dm/IGrid1D.hxx>
#include <bio/dm/ICursor1D.hxx>
#include <vector>

BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *  Bound solver. Mainly responsible for construction of the bound solver.
 *  \see ConstantCondition, WallCondition and MergeCondition.
 *
 */
class BoundSubSolver : public BIO_DM_NS::IGrid1D
{
private:

    Solver* solver;
    int positionH;
    int positionV;
    bool horizontal;
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer;
    BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer* fdAnalyzer;
    BIO_CFG_NS::BoundAnalyzer* boundAnalyzer;
    BIO_DM_NS::ISegmentSplit* segmentSplit;

    /**
     *  This array maps global substance indexes to a corresponding
     *  bound condition.
     */
    IBoundCondition* * substanceToBCMap;

    std::vector<IBoundCondition*> boundConditions;

    std::vector<IAreaEdgeData*> allocatedEdges;
    std::vector<IAreaEdgeFunction*> allocatedFunctions;

    typedef std::vector<IBoundCondition*>::iterator BCIterator;

public:

    /**
     *  Constructor. All initialization is done here.
     */
    BoundSubSolver(
        Solver* solver,
        unsigned positionH,
        unsigned positionV,
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

    /**
     *  Set new concentration.
     *  \param x    Point index (local).
     *  \param s    Substance index (global).
     *  \param c    New concentration.
     */
    void setConcentration(int x, int s, double c);

    /* ********************************************************************** */
    /* *********   IGrid1D implementation follows   ************************* */

    /**
     *  Returns number of substances defined in the model.
     */
    virtual int getSubstanceCount();

    /**
     *  Returns substance configurations.
     *
     *  \return Substance configuration or 0 if there is no bound conditions
     *          at this bouns.
     */
    virtual BIO_XML_MODEL_NS::Substance* getSubstanceConf(int index);

    /**
     *  Get positions of the area split points.
     */
    virtual BIO_DM_NS::ISegmentSplit* getPointPositions();

    /**
     *  Created new cursor, that can be used to get concentrations of the
     *  substances in this bound. Client of this object must delete this
     *  cursor.
     */
    virtual BIO_DM_NS::ICursor1D* newGridCursor();

    /* *********   IGrid1D implementation ends   **************************** */
    /* ********************************************************************** */

protected:

    /**
     *  Creates bound condition for particular specification of
     *  substance for bound.
     */
    void createBoundCondition(
        BIO_XML_MODEL_NS::BoundSubstance * boundSubstance,
        const std::vector<BIO_XML_MODEL_NS::Reaction*>& boundReactions,
        AreaSubSolver* areaPrev,
        AreaSubSolver* areaNext,
        int substance,
        bool atStart
    );

    /*
     *  FIXME: delete this.
     *
    IAreaEdgeData* createEdgeDataByReactions(
        IAreaEdgeData* baseEdgeData,
        int baseSubstance,
        AreaSubSolver* area,
        bool horizontal,
        bool atStart,
        const std::vector<BIO_XML_MODEL_NS::Reaction*>& boundReactions
    );
     */

    /* ********************************************************************** */
    /* ********************************************************************** */
    /**
     *  Cursor.
     */
    class Cursor : public BIO_DM_NS::ICursor1D, public BIO_DM_NS::IConcentrations
    {
    private:
        unsigned position;
        unsigned pointCount;
        BoundSubSolver* subSolver;

    public:
        Cursor(BoundSubSolver* subSolver);
        virtual ~Cursor();
        virtual void prev();
        virtual void next();
        virtual void start();
        virtual void end();
        virtual bool isValid();
        virtual IConcentrations* getConcentrations();

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


    /* ********************************************************************** */
    /* ********************************************************************** */
};


BIO_SLV_FD_IM2D_NS_END

#endif
