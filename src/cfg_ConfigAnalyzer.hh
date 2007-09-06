#ifndef CFG_ConfigAnalyzer_HH
#define CFG_ConfigAnalyzer_HH
#include <vector>
#include <log4cxx/logger.h>
#include "xsd/Model.hh"


namespace cfg
{



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Dvimates konfiguracijos analizatorius.
 */
class ConfigAnalyzer2D
{
protected:
    ::log4cxx::LoggerPtr logger;
    ::xsd::model::Model*  model;            ///< Model, that is analyzed
    ::xsd::model::Medium*** areaMatrix;     ///< Matrix with assigned mediums.
    ::xsd::model::BoundCondition**** boundConditionH;   ///< [x][y][s] Horizontal bounds
    ::xsd::model::BoundCondition**** boundConditionV;   ///< [x][y][s] Vertical bounds
    ::xsd::model::BoundInitial****   boundInitialH;     ///< [x][y][s] Horizontal bounds
    ::xsd::model::BoundInitial****   boundInitialV;     ///< [x][y][s] Vertical bounds
    int areaMatrixX;                        ///< Matrix size in X
    int areaMatrixY;                        ///< Matrix size in Y
    int substanceCount;                     ///< Substance count in model

    /// Krastines salygos, kurios buvo sugeneruotos modelio papildymo metu.
    std::vector< ::xsd::model::BoundCondition* > generatedBoundConditions;

public:
    ConfigAnalyzer2D(::xsd::model::Model* model);
    virtual ~ConfigAnalyzer2D();
    virtual void analyze();

protected:
    int boundIndexInAxis(
        const ::xsd::model::Axis &axis,
        const ::xsd::model::SymbolName &name
    );

    int substanceIndex(
        const ::xsd::model::Substance &substance
    );

    const ::xsd::model::MediumDiffusion* diffusionBySubstance(
        const ::xsd::model::Medium&    medium,
        const ::xsd::model::Substance& substance
    );

    xsd::model::BoundCondition* calculateBoundCondition(
        xsd::model::Medium* medium1,
        xsd::model::Medium* medium2,
        int substanceIdx
    );

};



/* ************************************************************************** */
/* ************************************************************************** */
}   // namespace cfg

#endif  // CFG_ConfigAnalyzer_HH
