#ifndef BIO_TRD_IntegratedReaction_HXX
#define BIO_TRD_IntegratedReaction_HXX
#include "../../biosensor.hxx"
#include "../cfg/StructureAnalyzer.hxx"
#include "../dm/IConcentrations.hxx"
#include "IIntegratedExpression.hxx"
#include <biosensor-xml.hxx>

BIO_TRD_NS_BEGIN


/**
 *  Expression used to integrate "speed" of the particular reaction.
 */
class IntegratedReaction : public IIntegratedExpression
{
protected:
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer;
    BIO_XML_MODEL_NS::Reaction* reaction;

public:

    /**
     *  Constructor.
     *
     *  \param structAnalyzer
     */
    IntegratedReaction(
        BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
        BIO_XML_MODEL_NS::ReactionName& reactionName
    );

    /**
     *  Destructor.
     */
    virtual ~IntegratedReaction();

    /**
     *  Is this expression defined in the area with specified position.
     *
     *  \param h    Position in the hozirontal axis.
     *  \param v    Position in the vertical axis.
     *  \return     True id expression is defined.
     */
    virtual bool isDefined(int h, int v);

    /**
     *  Factory method for the various reactions.
     */
    static IntegratedReaction* newInstance(
        BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
        BIO_XML_MODEL_NS::ReactionName& reactionName
    );

};



BIO_TRD_NS_END
#endif
