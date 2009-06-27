#ifndef BIO_CFG_BoundAnalyzer_HXX
#define BIO_CFG_BoundAnalyzer_HXX
#include "../../biosensor.hxx"
#include "StructureAnalyzer.hxx"
#include <biosensor-xml.hxx>
#include <vector>
BIO_CFG_NS_BEGIN


/**
 *  Analyzer, that provides all information, related to boundary conditions
 *  of the model. As an input it uses StructureAnalyzer.
 */
class BoundAnalyzer
{
private:

    /**
     *  This structure holds all info about one boundary condition,
     *  for one substance.
     */
    struct BoundSubstanceInfo
    {
        BIO_XML_MODEL_NS::BoundSubstance*        boundSubstance;
        BIO_XML_MODEL_NS::Bound*                 derivedFromBound;
        std::vector<BIO_XML_MODEL_NS::Reaction*> relatedReactions;
        BoundSubstanceInfo();
        BoundSubstanceInfo& operator = (BoundSubstanceInfo& source);
    };

    /**
     *  This structure hold info about all characreristics
     *  of the bound at the specified position.
     */
    class BoundInfo
    {
    private:
        BIO_XML_MODEL_NS::Bound* boundDefinition;
        std::vector<BoundSubstanceInfo> boundSubstances;
    public:
        BoundInfo();
        ~BoundInfo();
        BIO_XML_MODEL_NS::Bound* getBoundDefinition();
        void setBoundDefinition(BIO_XML_MODEL_NS::Bound* boundDef);
        void setSubstanceCount(int substCount);
        BoundSubstanceInfo& operator[] (int substIndex);
    };

    /**
     *  Analyzer, used to get all needed info about the model.
     */
    StructureAnalyzer *structAnalyzer;

    /**
     *  Array of bound definitions, structure is (BoundInfo[h][v][side]).
     *  Coordinates h and v are area coordinates and side is a side of an area,
     *  on which the bound condition if defined.
     */
    BoundInfo *** bounds; // [h][v][side]

    int sizeH;  //!< #boundSubstances size in h (number of areas horizontally)
    int sizeV;  //!< #boundSubstances size in v (number of areas vertically)
    int sizeS;  //!< #boundSubstances size in s

    /**
     *  A vector for the bounds, allocated by this class.
     *  It is used for a cleanup.
     */
    std::vector< BIO_XML_MODEL_NS::BoundSubstance* > allocatedBoundConditions;

public:

    enum AreaSide
    {
        TOP     = 0,
        BOTTOM  = 1,
        LEFT    = 2,
        RIGHT   = 3
    };

    /**
     *  Constructor.
     *  \param structAnalyzer Struture analyzer, created for this config.
     */
    BoundAnalyzer(StructureAnalyzer *structAnalyzer);

    /**
     *  Destructor.
     */
    virtual ~BoundAnalyzer();

    /**
     *  Get bound specification for specific area side and substance.
     *
     *  \param s    Substance index as returned by StructureAnalyzer::getSubstanceIndex
     *  \param h    Horizontal position of the area.
     *  \param v    Vertical position of the area.
     *  \param side Side of the area.
     *  \return     Bound specification for particular substance.
     */
    BIO_XML_MODEL_NS::BoundSubstance* getBoundForSubstance(int s, int h, int v, AreaSide side);

    /**
     *  Returns the name of the bound, that defined specified bound condition.
     *
     *  \param s    Substance index as returned by StructureAnalyzer::getSubstanceIndex
     *  \param h    Horizontal position of the area.
     *  \param v    Vertical position of the area.
     *  \param side Side of the area.
     *  \return Name of the bound. 0 is returned is a name was not specified or
     *          a bound condition was generated (guessed).
     */
    std::string* getBoundName(int s, int h, int v, AreaSide side);

    /**
     *  Returns reaction definitions, in which the substance participates.
     *
     *  \param s    Substance index as returned by StructureAnalyzer::getSubstanceIndex
     *  \param h    Horizontal position of the area.
     *  \param v    Vertical position of the area.
     *  \param side Side of the area.
     *  \return List of reactions.
     */
    const std::vector<BIO_XML_MODEL_NS::Reaction*>& getRelatedReactions(int s, int h, int v, AreaSide side) const;

private:

    /**
     *  Apply all bound conditions on one side of an area for all substances,
     *  specified in the bound definition.
     */
    void applyBoundConditions(
        int h,
        int v,
        AreaSide side,
        BIO_XML_MODEL_NS::Bound* bound
    );

    /**
     *  Apply one bound condition: for the particular substance,
     *  at the specified position.
     */
    void applyBoundCondition(
        int h,
        int v,
        int s,
        AreaSide side,
        BIO_XML_MODEL_NS::Bound* bProvided,
        BIO_XML_MODEL_NS::BoundSubstance* bsProvided
    );

    /**
     *  Collects the reactions, that are involving the specified substance.
     */
    void collectRelatedReactions(
        BoundInfo& bi,
        BoundSubstanceInfo& bsi
    );

};


BIO_CFG_NS_END
#endif
