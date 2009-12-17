#include "SubstanceGradOnEdge.hxx"


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::SubstanceGradOnEdge::SubstanceGradOnEdge(
    BIO_SLV_FD_IM2D_NS::IAreaEdgeData* edgeData,
    double diffusionCoef
)
{
    this->edgeData = edgeData;
    this->coef = diffusionCoef / edgeData->getStepSize();

    if (!edgeData->isForward())
    {
        coef = -coef;
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::SubstanceGradOnEdge::~SubstanceGradOnEdge()
{
    //  Nothing to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
int BIO_SLV_FD_IM2D_NS::SubstanceGradOnEdge::getSize()
{
    return edgeData->getSize();
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_SLV_FD_IM2D_NS::SubstanceGradOnEdge::getValue(int index)
{
    return coef * (edgeData->getC1(index) - edgeData->getC0(index));
}


/* ************************************************************************** */
/* ************************************************************************** */
