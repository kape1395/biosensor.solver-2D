#include "IntegratedConcentration.hxx"
#include <vector>
#include <algorithm>


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::IntegratedConcentration::IntegratedConcentration(
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
    BIO_XML_MODEL_NS::SubstanceName& substanceName
) : IIntegratedExpression()
{
    this->structAnalyzer = structAnalyzer;
    this->substanceIndex = structAnalyzer->getSubstanceIndex(substanceName);
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::IntegratedConcentration::~IntegratedConcentration()
{
    //  Nothiong to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
bool BIO_TRD_NS::IntegratedConcentration::isDefined(int h, int v)
{
    std::vector<int> s = structAnalyzer->getSubstanceIndexesInArea(h, v);
    return std::find(s.begin(), s.end(), substanceIndex) != s.end();
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_TRD_NS::IntegratedConcentration::getValue(BIO_DM_NS::IConcentrations* c)
{
    return c->getConcentration(substanceIndex);
}


/* ************************************************************************** */
/* ************************************************************************** */
