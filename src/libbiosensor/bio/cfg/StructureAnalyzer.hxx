#ifndef BIO_CFG_StructureAnalyzer_HXX
#define BIO_CFG_StructureAnalyzer_HXX
#include "../../biosensor.hxx"
#include <biosensor-xml.hxx>
#include <vector>
#include <log4cxx/logger.h>
BIO_CFG_NS_BEGIN


/**
 *  Simplifies retrieval of the needed parameters from model.
 *  This class is just helper now, but in the future it can help to hide
 *  configuration xml-format from the rest of the code.
 */
class StructureAnalyzer
{
private:
    log4cxx::LoggerPtr log;

    BIO_XML_NS::model::Model* config;
    bool twoDimensional;
    std::vector< BIO_XML_NS::model::Symbol* > pointsH;
    std::vector< BIO_XML_NS::model::Symbol* > pointsV;
    std::vector< BIO_XML_NS::model::Substance* > substances;
    std::vector< BIO_XML_NS::model::MediumReaction* >** reactions;
    BIO_XML_NS::model::Symbol* *** diffusions;                      // Symbol* [h][v][s]
    BIO_XML_NS::model::Symbol* *** initialConcentrations;           // Symbol* [h][v][s]
    BIO_XML_NS::model::Medium* ** mediums;                          // Medium* [h][v]

    /// Position for the case, when these was Linear (one-dimensional) coordinate system.
    BIO_XML_MODEL_NS::Symbol axisPoint0;
    BIO_XML_MODEL_NS::Symbol diffusion0;

public:

    /**
     *  Axis types.
     */
    enum Axis
    {
        HORIZONTAL = 0,
        VERTICAL   = 1
    };

    /**
     *  Constructor.
     *  \param config  Configuration to be analyzed.
     */
    StructureAnalyzer(BIO_XML_NS::model::Model* config);

    /**
     *  Destructor.
     */
    virtual ~StructureAnalyzer();

    /**
     *  Returns configuration used by this analyzer.
     *  \return Model of the biosensor.
     */
    BIO_XML_NS::model::Model* getConfig()
    {
        return config;
    }

    /**
     *  Returns true if analyzed model is two-dimensional and
     *  false in another case (1D model).
     */
    bool isTwoDimensional()
    {
        return twoDimensional;
    }

    bool isCoordinateSystemCartesian()
    {
        return config->coordinateSystem() == BIO_XML_NS::model::CoordinateSystem::Cartesian;
    }

    bool isCoordinateSystemCylindrical()
    {
        return config->coordinateSystem() == BIO_XML_NS::model::CoordinateSystem::Cylindrical;
    }

    /**
     *  Returns points in horizontal (x) axis.
     *
     *  \return List of point definitions.
     */
    std::vector< BIO_XML_NS::model::Symbol* >& getPointsH()
    {
        return pointsH;
    }

    /**
     *  Returns points in vertical (y) axis.
     *
     *  \return List of point definitions.
     */
    std::vector< BIO_XML_NS::model::Symbol* >& getPointsV()
    {
        return pointsV;
    }

    /**
     *  Returns substances, that are mentioned in the model.
     *  The ordering of the substances (and indexes of them) are defined
     *  by this method.
     *
     *  \return List of substance descriptions.
     */
    std::vector< BIO_XML_NS::model::Substance* >& getSubstances()
    {
        return substances;
    }

    /**
     *  Returns substance index by the substance name.
     *  First substance index is 0.
     *
     *  \return substance index.
     *  \throws Exception, when substance not found.
     */
    int getSubstanceIndex(BIO_XML_NS::model::SubstanceName& name);

    /**
     *  Returns indexes of the substances, that are used in the specified area.
     *
     *  \param h Horizontal (x) coordinate of the area.
     *  \param v Vertical (y) coordinate of the area.
     *  \return vector of indexes (see also getSubstanceIndex).
     */
    std::vector<int> getSubstanceIndexesInArea(int h, int v);

    /**
     *  Get the reactions in the specified area.
     *
     *  \param h Horizontal (x) coordinate of the area.
     *  \param v Vertical (y) coordinate of the area.
     *  \return List of reactions. This list can also be empty (size = 0).
     */
    std::vector< BIO_XML_NS::model::MediumReaction* >& getReactions(int h, int v)
    {
        return reactions[h][v];
    }

    /**
     *  Get diffusion coefficient of the substance s
     *  at the specified area.
     *
     *  \param h Horizontal (x) coordinate of the area.
     *  \param v Vertical (y) coordinate of the area.
     *  \param s Substance index.
     *  \return Diffusion coefficient or 0, if there is no
     *      diffusion of specified substance in that area.
     */
    BIO_XML_NS::model::Symbol* getDiffusion(int s, int h, int v)
    {
        return diffusions[h][v][s];
    }

    /**
     *  Get initial concentrations of the substance s
     *  at the specified area.
     *
     *  \param h Horizontal (x) coordinate of the area.
     *  \param v Vertical (y) coordinate of the area.
     *  \param s Substance index.
     *  \return Initial concentration or 0, if there is no
     *      diffusion of specified substance in that area.
     */
    BIO_XML_NS::model::Symbol* getInitialConcentration(int s, int h, int v)
    {
        return initialConcentrations[h][v][s];
    }

    /**
     *  \deprecated Use more concrete methods instead of this.
     *  Returns medium by the specified position.
     *
     *  \param h Horizontal (x) coordinate of the area.
     *  \param v Vertical (y) coordinate of the area.
     *  \return Medium definition or 0, if no Medium
     *      is defined in that area.
     */
    BIO_XML_NS::model::Medium* getMedium(int h, int v)
    {
        return mediums[h][v];
    }

    /**
     *  Returns a medium name for a specified area.
     *
     *  \param h Horizontal (x) coordinate of the area.
     *  \param v Vertical (y) coordinate of the area.
     *  \return MediumName or 0, if no Medium or name exists for that area.
     */
    BIO_XML_MODEL_NS::MediumName* getMediumName(int h, int v);

    /**
     *  Finds symbol definition by the name.
     *
     *  \param name Symbol name.
     *  \return Symbol definition or 0 if symbol not found.
     */
    BIO_XML_NS::model::Symbol* getSymbol(BIO_XML_NS::model::SymbolName& name);

    /**
     *  Check if specified symbol is a point in the specified axis.
     *
     *  \param axis             Axis to check.
     *  \param pointSymbolName  Name of the point symbol.
     *  \returns true if point found and false otherwise.
     */
    bool isPointInAxis(
        Axis axis,
        BIO_XML_NS::model::SymbolName& pointSymbolName
    );


    /**
     *  Returns index of point in axis by point name.
     *
     *  \param axis             Axis, in which point should be found.
     *  \param pointSymbolName  Name of the point in axis.
     *  \return index, starting at 0.
     *  \throws Exception If point not found in axis.
     */
    int getPointIndexInAxis(
        Axis axis,
        BIO_XML_NS::model::SymbolName& pointSymbolName
    );

private:

    /**
     *  Fills given list with symbols, mentioned in the axis definition.
     */
    void fillListWithAxisPoints(
        std::vector< BIO_XML_NS::model::Symbol* >& list,
        BIO_XML_NS::model::Axis& axis,
        bool inverted = false
    );

    /**
     *  Returns index of point in axis by point name.
     *
     *  \param axis             Axis, in which point should be found.
     *  \param pointSymbolName  Name of the point in axis.
     *  \return index, starting at 0.
     *  \throws Exception If point not found in axis.
     */
    int getPointIndexInAxis(
        std::vector< BIO_XML_NS::model::Symbol* >& axis,
        std::string& pointSymbolName
    );

};


BIO_CFG_NS_END
#endif
