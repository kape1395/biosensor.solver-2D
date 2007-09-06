#ifndef CFG_ConfigAnalyzer_HH
#define CFG_ConfigAnalyzer_HH
#include <log4cxx/logger.h>
#include "xsd/Model.hh"


namespace cfg
{



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *
 */
class ConfigAnalyzer2D
{
protected:
    ::log4cxx::LoggerPtr logger;
    ::xsd::model::Model  *model;            ///< Model, that is analyzed
    ::xsd::model::Medium ***areaMatrix;     ///< Matrix with assigned mediums.
    ::xsd::model::BoundConstraint ***boundsH;
    ::xsd::model::BoundConstraint ***boundsV;
    int areaMatrixX;                        ///< Matrix size in X
    int areaMatrixY;                        ///< Matrix size in Y

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
};



/* ************************************************************************** */
/* ************************************************************************** */
}   // namespace cfg

#endif  // CFG_ConfigAnalyzer_HH
