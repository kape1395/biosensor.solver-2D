/*
 * Copyright 2011 Karolis Petrauskas
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#ifndef BIO_CFG_StructureAnalyzer_HXX
#define BIO_CFG_StructureAnalyzer_HXX
#include "../../biosensor.hxx"
#include <biosensor-xml.hxx>
#include <vector>
BIO_CFG_NS_BEGIN


/**
 *  Simplifies retrieval of the needed parameters from model.
 *  This class is just helper now, but in the future it can help to hide
 *  configuration xml-format from the rest of the code.
 */
class StructureAnalyzer
{
private:

    BIO_XML_MODEL_NS::Model* config;
    bool twoDimensional;
    std::vector< BIO_XML_MODEL_NS::Symbol* > symbols;
    std::vector< BIO_XML_MODEL_NS::Symbol* > pointsH;
    std::vector< BIO_XML_MODEL_NS::Symbol* > pointsV;
    std::vector< BIO_XML_MODEL_NS::Substance* > substances;
    std::vector< BIO_XML_MODEL_NS::Reaction* >** reactions;
    BIO_XML_MODEL_NS::Symbol* *** diffusions;                      // Symbol* [h][v][s]
    BIO_XML_MODEL_NS::Symbol* *** initialConcentrations;           // Symbol* [h][v][s]
    BIO_XML_MODEL_NS::Medium* ** mediums;                          // Medium* [h][v]

    /// Position for the case, when these was Linear (one-dimensional) coordinate system.
    BIO_XML_MODEL_NS::Symbol axisPoint0;
    BIO_XML_MODEL_NS::Symbol axisPoint1;
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
    StructureAnalyzer(BIO_XML_MODEL_NS::Model* config);

    /**
     *  Destructor.
     */
    virtual ~StructureAnalyzer();

    /**
     *  Returns configuration used by this analyzer.
     *  \return Model of the biosensor.
     */
    BIO_XML_MODEL_NS::Model* getConfig()
    {
        return config;
    }

    /**
     *  Returns true if analyzed model is two-dimensional and
     *  false in another case (1D model).
     *  One-dimensional model can be threated as a two-dimensional one, with
     *  its width = 1. Thats why this method always returns true.
     */
    bool isTwoDimensional()
    {
        return true;
    }

    /**
     *  Returns true, if model can be simulated using one-dimensional solver.
     *  In one-dimensional model, the x axis is "vertical".
     */
    bool isOneDimensional()
    {
        return pointsH.size() == 2;
    }

    /**
     *  Returns true, if model is formulated in the onedimensional coordinate system.
     */
    bool isCoordinateSystemLinear()
    {
        return config->coordinateSystem() == BIO_XML_MODEL_NS::CoordinateSystem::Linear;
    }

    /**
     *  Returns true, if model is formulated in the one or two-dimensional
     *  coordinate system.
     */
    bool isCoordinateSystemCartesian()
    {
        return config->coordinateSystem() == BIO_XML_MODEL_NS::CoordinateSystem::Cartesian ||
               config->coordinateSystem() == BIO_XML_MODEL_NS::CoordinateSystem::Linear ;
    }

    /**
     *  Returns true, if model is formulated in the r-z plane of the cylindrical
     *  coordinate system.
     */
    bool isCoordinateSystemCylindrical()
    {
        return config->coordinateSystem() == BIO_XML_MODEL_NS::CoordinateSystem::Cylindrical;
    }

    /**
     *  Returns points in horizontal (x) axis.
     *
     *  \return List of point definitions.
     */
    std::vector< BIO_XML_MODEL_NS::Symbol* >& getPointsH()
    {
        return pointsH;
    }

    /**
     *  Returns points in vertical (y) axis.
     *
     *  \return List of point definitions.
     */
    std::vector< BIO_XML_MODEL_NS::Symbol* >& getPointsV()
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
    std::vector< BIO_XML_MODEL_NS::Substance* >& getSubstances()
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
    int getSubstanceIndex(BIO_XML_MODEL_NS::SubstanceName& name);

    /**
     *  Returns indexes of the substances, that are used in the specified area.
     *
     *  \param h Horizontal (x) coordinate of the area.
     *  \param v Vertical (y) coordinate of the area.
     *  \return vector of indexes (see also getSubstanceIndex).
     */
    std::vector<int> getSubstanceIndexesInArea(int h, int v);

    /**
     *  Checks if the substance is defined in the area with specified position.
     *
     *  \param s Substance index.
     *  \param h Horizontal (x) coordinate of the area.
     *  \param v Vertical (y) coordinate of the area.
     *  \return true, if substance is defined in the area.
     */
    bool substanceExistsInArea(int s, int h, int v);

    /**
     *  Returns indexes of the substances, that are defined in the specified medium.
     *
     *  \param name Medium name.
     *  \return vector of indexes (see also getSubstanceIndex).
     */
    std::vector<int> getSubstanceIndexesInMedium(BIO_XML_MODEL_NS::MediumName& name);

    /**
     *  Returns a reaction definition by the name.
     */
    BIO_XML_MODEL_NS::Reaction* getReaction(BIO_XML_MODEL_NS::ReactionName& name);

    /**
     *  Get the reactions in the specified area.
     *
     *  \param h Horizontal (x) coordinate of the area.
     *  \param v Vertical (y) coordinate of the area.
     *  \return List of reactions. This list can also be empty (size = 0).
     */
    std::vector< BIO_XML_MODEL_NS::Reaction* >& getReactions(int h, int v)
    {
        return reactions[h][v];
    }

    /**
     *  Get diffusion coefficient of the substance s
     *  at the specified area and direction.
     *
     *  \param h Horizontal (x) coordinate of the area.
     *  \param v Vertical (y) coordinate of the area.
     *  \param s Substance index.
     *  \param horizontal
     *      Direction for which the diffusion coefficient must be returned.
     *      If true, direction is horizontal, and vertical if false.
     *  \return Diffusion coefficient or 0.0, if there is no
     *      diffusion of specified substance in that area.
     *      This diffusion coefficient is alredy multipied by
     *      the diffusion ratio.
     */
    double getDiffusionCoef(int s, int h, int v, bool horizontal);

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
    BIO_XML_MODEL_NS::Symbol* getInitialConcentration(int s, int h, int v)
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
    BIO_XML_MODEL_NS::Medium* getMedium(int h, int v)
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
    BIO_XML_MODEL_NS::Symbol* getSymbol(BIO_XML_MODEL_NS::SymbolName& name);

    /**
     *  Check if specified symbol is a point in the specified axis.
     *
     *  \param axis             Axis to check.
     *  \param pointSymbolName  Name of the point symbol.
     *  \returns true if point found and false otherwise.
     */
    bool isPointInAxis(
        Axis axis,
        BIO_XML_MODEL_NS::SymbolName& pointSymbolName
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
        BIO_XML_MODEL_NS::SymbolName& pointSymbolName
    );

private:

    /**
     *  Fills given list with symbols, mentioned in the axis definition.
     */
    void fillListWithAxisPoints(
        std::vector< BIO_XML_MODEL_NS::Symbol* >& list,
        BIO_XML_MODEL_NS::Axis& axis,
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
        std::vector< BIO_XML_MODEL_NS::Symbol* >& axis,
        std::string& pointSymbolName
    );

};


BIO_CFG_NS_END
#endif
