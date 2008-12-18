#ifndef BIO_CFG_StructureAnalyzer_HXX
#define BIO_CFG_StructureAnalyzer_HXX
#include "../../biosensor.hxx"
#include "IConfigAnalyzer.hxx"
#include "../Exception.hxx"
#include <biosensor-xml.hxx>
#include <vector>
#include <log4cxx/logger.h>
BIO_CFG_NS_BEGIN


/**
 *  Simplifies retrieval of the needed parameters from model.
 *  This class is just helper now, but in the future it can help to hide
 *  configuration xml-format from the rest of the code.
 */
class StructureAnalyzer : public IConfigAnalyzer
{
private:
    log4cxx::LoggerPtr log;

    BIO_XML_NS::model::Model* config;
    bool twoDimensional;
    std::vector< BIO_XML_NS::model::Symbol* > pointsH;
    std::vector< BIO_XML_NS::model::Symbol* > pointsV;
    std::vector< BIO_XML_NS::model::Substance* > substances;
    std::vector< BIO_XML_NS::model::MediumReaction* >** reactions;
    BIO_XML_NS::model::Symbol* *** diffusions;
    BIO_XML_NS::model::Symbol* *** initialConcentrations;
    BIO_XML_NS::model::Medium* ** mediums;

    /// Position for the case, when these was Linear (one-dimensional) coordinate system.
    BIO_XML_NS::model::Symbol axisPoint0;

public:

    /**
     *  Constructor.
     */
    StructureAnalyzer() :
            log(log4cxx::Logger::getLogger("libbiosensor::StructureAnalyzer")),
            axisPoint0(BIO_XML_NS::model::SymbolName("axisPoint0"), 0)
    {
        twoDimensional = false; // it is not very correct, but...
        reactions = 0;
        diffusions = 0;
        initialConcentrations = 0;
        mediums = 0;
    }

    /**
     *  Destructor.
     */
    virtual ~StructureAnalyzer()
    {
        analyze(0);
    }

    /**
     *  Setter for configuration, to be analyzed.
     *  Should analysis be performed in this method?
     *
     *  \param config  Configuration to be analyzed. This also can be 0.
     */
    virtual void analyze(BIO_XML_NS::model::Model* config);

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
     *  Finds symbol definition by the name.
     *
     *  \param name Symbol name.
     *  \return Symbol definition or 0 if symbol not found.
     */
    BIO_XML_NS::model::Symbol* getSymbol(std::string& name);

private:

    /**
     *  Fills given list with symbols, mentioned in the axis definition.
     */
    void fillListWithAxisPoints(std::vector< BIO_XML_NS::model::Symbol* >& list, BIO_XML_NS::model::Axis& axis);

    /**
     *  Returns index of point in axis by point name.
     */
    int getPointIndexInAxis(std::vector< BIO_XML_NS::model::Symbol* >& axis, std::string& pointSymbolName);

};


BIO_CFG_NS_END
#endif
