#ifndef BIO_CFG_ReactionAnalyzer_HXX
#define BIO_CFG_ReactionAnalyzer_HXX
#include "../../biosensor.hxx"
#include <biosensor-xml.hxx>
BIO_CFG_NS_BEGIN


/**
 *  Halper class to analyze reactions.
 */
class ReactionAnalyzer
{
public:
    static ReactionAnalyzer* newAnalyzer(BIO_XML_MODEL_NS::Reaction* reaction);

    virtual ~ReactionAnalyzer()
    {
        //  Nothing.
    }

    virtual bool isSubstrate(BIO_XML_MODEL_NS::SubstanceName& substance) = 0;
    virtual bool isProduct(BIO_XML_MODEL_NS::SubstanceName& substance) = 0;
};


BIO_CFG_NS_END
#endif
