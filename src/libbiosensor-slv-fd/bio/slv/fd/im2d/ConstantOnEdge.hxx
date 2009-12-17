#ifndef BIO_SLV_FD_IM2D_ConstantOnEdge_HXX
#define BIO_SLV_FD_IM2D_ConstantOnEdge_HXX
#include "../../../../biosensor-slv-fd.hxx"
#include "IAreaEdgeFunction.hxx"
BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *  This class is a constant fonction on the bound of the area.
 */
class ConstantOnEdge : public BIO_SLV_FD_IM2D_NS::IAreaEdgeFunction
{
    int size;
    double value;

public:

    /**
     *  Constructor.
     *  @param size     Lenght of the edge.
     *  @param value    Constant value.
     */
    ConstantOnEdge(int size, double value);

    /**
     *  Destructor.
     */
    virtual ~ConstantOnEdge();

    virtual int getSize();

    virtual double getValue(int index);

};

BIO_SLV_FD_IM2D_NS_END

#endif
