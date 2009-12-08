#ifndef BIO_TRD_IntegralOverArea_HXX
#define BIO_TRD_IntegralOverArea_HXX
#include "../../biosensor.hxx"
#include "../cfg/StructureAnalyzer.hxx"
#include "../slv/ISolver.hxx"
#include "../dm/IGrid2D.hxx"
#include "IIntegratedExpression.hxx"
#include <biosensor-xml.hxx>
#include <string>
#include <vector>
BIO_TRD_NS_BEGIN


/**
 *  Calculates an integral for specifled IntegratedExpression.
 */
class IntegralOverArea
{
private:
    const double CONST_PI;

    BIO_CFG_NS::StructureAnalyzer* structAnalyzer;

    IIntegratedExpression* expression;

    /**
     *  Arreas to be integrated.
     */
    std::vector<BIO_DM_NS::IGrid2D*> areas;

public:

    /**
     *  Construct integrator by medium and substance name.
     */
    IntegralOverArea(
        BIO_SLV_NS::ISolver*            solver,
        BIO_XML_MODEL_NS::MediumName&   mediumName,
        IIntegratedExpression*          expression,
        BIO_CFG_NS::StructureAnalyzer*  structAnalyzer
    );

    /**
     *  Construct integrator for substance concentration over all biosensor.
     */
    IntegralOverArea(
        BIO_SLV_NS::ISolver*            solver,
        IIntegratedExpression*          expression,
        BIO_CFG_NS::StructureAnalyzer*  structAnalyzer
    );

    /**
     *  Destructor.
     */
    virtual ~IntegralOverArea();

    /**
     *  Calculate integral of concentration over area.
     *
     *  \returns integral value.
     */
    virtual double integrate();

    /**
     *
     */
    virtual double integrateOverVolume();

    /**
     *  Returns area of the region, over which integration is performed.
     */
    virtual double getVolume();

    /**
     *  returns sub-areas, over which integrations is performed.
     */
    virtual const std::vector<BIO_DM_NS::IGrid2D*>& getIntegratedAreas()
    {
        return areas;
    }

    /**
     *  Returns integrated expression.
     */
    virtual IIntegratedExpression* getExpression()
    {
        return expression;
    }

private:

    /**
     *  Calculated integral for one sub-area.
     */
    double integrateSubArea(BIO_DM_NS::IGrid2D* area);

};



BIO_TRD_NS_END
#endif
