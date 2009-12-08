#ifndef BIO_TRD_IntegratedReactionRedOx_HXX
#define BIO_TRD_IntegratedReaction_HXX
#include "../../biosensor.hxx"
#include "../dm/IConcentrations.hxx"
#include "IntegratedReaction.hxx"
#include <biosensor-xml.hxx>

BIO_TRD_NS_BEGIN


/**
 *  Expression used to integrate "speed" of the particular RedOx reaction.
 */
class IntegratedReactionRedOx : public IntegratedReaction
{
private:
    std::vector<int> substrates;
    double rate;

public:

    /**
     *  Constructor.
     *
     *  \param structAnalyzer
     */
    IntegratedReactionRedOx(
        BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
        BIO_XML_MODEL_NS::ReactionName& reactionName
    );

    /**
     *  Destructor.
     */
    virtual ~IntegratedReactionRedOx();

    virtual double getValue(BIO_DM_NS::IConcentrations* c);

};



BIO_TRD_NS_END
#endif
