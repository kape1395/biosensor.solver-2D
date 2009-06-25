#ifndef BIO_TRD_IntegratedConcentration_HXX
#define BIO_TRD_IntegratedConcentration_HXX
#include "../../biosensor.hxx"
#include "../cfg/StructureAnalyzer.hxx"
#include "../dm/IConcentrations.hxx"
#include "IIntegratedExpression.hxx"
#include <biosensor-xml.hxx>

BIO_TRD_NS_BEGIN


/**
 *  Expression used to integrate concentration of the particular substance.
 */
class IntegratedConcentration : public IIntegratedExpression
{
private:
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer;
    int substanceIndex;

public:

    /**
     *  Constructor.
     *
     *  \param structAnalyzer
     *  \param substanceName
     */
    IntegratedConcentration(
        BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
        BIO_XML_MODEL_NS::SubstanceName& substanceName
    );

    /**
     *  Destructor.
     */
    virtual ~IntegratedConcentration();

    /**
     *  Is this expression defined in the area with specified position.
     *
     *  \param h    Position in the hozirontal axis.
     *  \param v    Position in the vertical axis.
     *  \return     True id expression is defined.
     */
    virtual bool isDefined(int h, int v);

    /**
     *  Returns value of the expression in the concrete point.
     *  This operation will be called only for points, where expression is defined.
     *
     *  \param c    Concentration values.
     *  \return     Value of the expression.
     */
    virtual double getValue(BIO_DM_NS::IConcentrations* c);

};



BIO_TRD_NS_END
#endif
