#ifndef BIO_TRD_ConcentrationIntegralOverArea_HXX
#define BIO_TRD_ConcentrationIntegralOverArea_HXX
#include "../../biosensor.hxx"
#include "../cfg/StructureAnalyzer.hxx"
#include "../slv/ISolver.hxx"
#include "../dm/IGrid2D.hxx"
#include <biosensor-xml.hxx>
#include <string>
#include <vector>
BIO_TRD_NS_BEGIN


/**
 *  Transducer: InjectedElectrode.
 */
class ConcentrationIntegralOverArea
{
private:
    double CONST_PI;

    BIO_CFG_NS::StructureAnalyzer* structAnalyzer;

    BIO_XML_MODEL_NS::SubstanceName substanceName;
    int substanceIndex;

    /**
     *  Arreas to be integrated.
     */
    std::vector<BIO_DM_NS::IGrid2D*> areas;

public:

    /**
     *  Construct integrator by medium and substance name.
     */
    ConcentrationIntegralOverArea(
        BIO_SLV_NS::ISolver* solver,
        BIO_XML_MODEL_NS::MediumName& mediumName,
        BIO_XML_MODEL_NS::SubstanceName& substanceName,
        BIO_CFG_NS::StructureAnalyzer* structAnalyzer
    );

    /**
     *  Construct integrator for substance concentration over all biosensor.
     */
    ConcentrationIntegralOverArea(
        BIO_SLV_NS::ISolver* solver,
        BIO_XML_MODEL_NS::SubstanceName& substanceName,
        BIO_CFG_NS::StructureAnalyzer* structAnalyzer
    );

    /**
     *  Destructor.
     */
    virtual ~ConcentrationIntegralOverArea();

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
     *
     *  \return area.
     */
    virtual double getArea();

    /**
     *
     */
    virtual double getVolume();

    /**
     *  returns sub-areas, over which integrations is performed.
     */
    virtual const std::vector<BIO_DM_NS::IGrid2D*>& getIntegratedAreas()
    {
        return areas;
    }

private:

    /**
     *  Calculated integral for one sub-area.
     */
    double integrateSubArea(BIO_DM_NS::IGrid2D* area);

};



BIO_TRD_NS_END
#endif
