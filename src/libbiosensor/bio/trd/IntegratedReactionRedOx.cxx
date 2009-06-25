#include "IntegratedReaction.hxx"
#include "../Exception.hxx"
#include "IntegratedReactionRedOx.hxx"
#include <vector>
#include <algorithm>


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::IntegratedReactionRedOx::IntegratedReactionRedOx(
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
    BIO_XML_MODEL_NS::ReactionName& reactionName
) : IntegratedReaction(structAnalyzer, reactionName)
{
    using namespace BIO_XML_MODEL_NS::reaction;

    ReductionOxidation* ro = dynamic_cast<ReductionOxidation*>(reaction);

    if (!ro)
        throw Exception("Only integral for ReductionOxidation is supported by this class");

    rate = structAnalyzer->getSymbol(ro->rate())->value();

    for (ReductionOxidation::substrate_iterator s = ro->substrate().begin(); s != ro->substrate().end(); s++)
    {
        substrates.push_back(structAnalyzer->getSubstanceIndex(s->name()));
    }

    if (substrates.size() < 1)
        throw Exception("At least 1 substrate must exist in the RedOx reaction");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::IntegratedReactionRedOx::~IntegratedReactionRedOx()
{
    //  Nothiong to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_TRD_NS::IntegratedReactionRedOx::getValue(BIO_DM_NS::IConcentrations* c)
{
    double value = rate;
    for (std::vector<int>::iterator s = substrates.begin(); s != substrates.end(); s++)
    {
        value *= c->getConcentration(*s);
    }
    return value;
}


/* ************************************************************************** */
/* ************************************************************************** */
