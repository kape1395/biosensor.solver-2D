#ifndef BIO_SLV_FD_IM2D_SubstanceGradOnEdge_HXX
#define BIO_SLV_FD_IM2D_SubstanceGradOnEdge_HXX
#include "../../../../biosensor-slv-fd.hxx"
#include "IAreaEdgeFunction.hxx"
#include "IAreaEdgeData.hxx"
BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *  This class calculates a gradient for the substance on the bound of
 *  the area.
 */
class SubstanceGradOnEdge : public BIO_SLV_FD_IM2D_NS::IAreaEdgeFunction
{
    BIO_SLV_FD_IM2D_NS::IAreaEdgeData* edgeData;
    double coef;    //!< Just a precalculated static coefficient.

public:

    /**
     *  Constructor.
     *  @param edgeData concentrations will be taken fron this source.
     *  @param diffusionCoef Dissusion coefficient of the substance.
     */
    SubstanceGradOnEdge(
        BIO_SLV_FD_IM2D_NS::IAreaEdgeData* edgeData,
        double diffusionCoef
    );

    /**
     *  Destructor.
     */
    virtual ~SubstanceGradOnEdge();

    virtual int getSize();

    virtual double getValue(int index);

};

BIO_SLV_FD_IM2D_NS_END

#endif
