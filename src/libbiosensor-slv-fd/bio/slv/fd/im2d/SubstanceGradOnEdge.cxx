#include "SubstanceGradOnEdge.hxx"
#include <bio/Logging.hxx>
#define LOGGER "libbiosensor-slv-fd::im2d::SubstanceGradOnEdge: "


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

    LOG_DEBUG(LOGGER
              << "Created: diffusionCoef=" << diffusionCoef
              << " isForward=" << edgeData->isForward()
             );
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
