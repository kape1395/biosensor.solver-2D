#include "ReactionAnalyzer.hxx"
#include "../Exception.hxx"


BIO_CFG_NS_BEGIN

/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  ReactionAnalyzer, which analyzes MichaelisMenten reactions.
 */
class ReactionAnalyzerMM : public BIO_CFG_NS::ReactionAnalyzer
{
private:
    BIO_XML_MODEL_NS::reaction::MichaelisMenten* reaction;

public:

    ReactionAnalyzerMM(BIO_XML_MODEL_NS::reaction::MichaelisMenten* reaction) : ReactionAnalyzer()
    {
        this->reaction = reaction;
    }

    virtual ~ReactionAnalyzerMM()
    {
        //  Nothing.
    }

    virtual bool isSubstrate(BIO_XML_MODEL_NS::SubstanceName& substance)
    {
        return reaction->substrate().compare(substance) == 0;
    }

    virtual bool isProduct(BIO_XML_MODEL_NS::SubstanceName& substance)
    {
        return reaction->product().compare(substance) == 0;
    }

};
BIO_CFG_NS_END

BIO_CFG_NS_BEGIN
/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  ReactionAnalyzer, which analyzes MichaelisMenten reactions.
 */
class ReactionAnalyzerRO : public BIO_CFG_NS::ReactionAnalyzer
{
private:
    BIO_XML_MODEL_NS::reaction::ReductionOxidation* reaction;

public:

    ReactionAnalyzerRO(BIO_XML_MODEL_NS::reaction::ReductionOxidation* reaction) : ReactionAnalyzer()
    {
        this->reaction = reaction;
    }

    virtual ~ReactionAnalyzerRO()
    {
        //  Nothing.
    }

    virtual bool isSubstrate(BIO_XML_MODEL_NS::SubstanceName& substance)
    {
        BIO_XML_MODEL_NS::reaction::ReductionOxidation::substrate_iterator it;
        BIO_XML_MODEL_NS::reaction::ReductionOxidation::substrate_sequence seq;
        for (seq = reaction->substrate(), it = seq.begin(); it != seq.end(); it++)
        {
            if (it->name().compare(substance) == 0)
                return true;
        }
        return false;
    }

    virtual bool isProduct(BIO_XML_MODEL_NS::SubstanceName& substance)
    {
        BIO_XML_MODEL_NS::reaction::ReductionOxidation::product_iterator it;
        BIO_XML_MODEL_NS::reaction::ReductionOxidation::product_sequence seq;
        for (seq = reaction->product(), it = seq.begin(); it != seq.end(); it++)
        {
            if (it->name().compare(substance) == 0)
                return true;
        }
        return false;
    }

};
BIO_CFG_NS_END


/* ************************************************************************** */
/* ************************************************************************** */
BIO_CFG_NS::ReactionAnalyzer* BIO_CFG_NS::ReactionAnalyzer::newAnalyzer(BIO_XML_MODEL_NS::Reaction* reaction)
{
    if (dynamic_cast<BIO_XML_MODEL_NS::reaction::ReductionOxidation*>(reaction))
    {
        return new BIO_CFG_NS::ReactionAnalyzerRO(dynamic_cast<BIO_XML_MODEL_NS::reaction::ReductionOxidation*>(reaction));
    }
    else if (dynamic_cast<BIO_XML_MODEL_NS::reaction::MichaelisMenten*>(reaction))
    {
        return new BIO_CFG_NS::ReactionAnalyzerMM(dynamic_cast<BIO_XML_MODEL_NS::reaction::MichaelisMenten*>(reaction));
    }
    else
    {
        throw Exception("Reaction analyzer does not know the supplies reaction.");
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
