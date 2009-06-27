#ifndef BIO_SLV_FD_IM2D_IAreaEdgeData_HXX
#define BIO_SLV_FD_IM2D_IAreaEdgeData_HXX
#include "../../../../biosensor-slv-fd.hxx"

BIO_SLV_FD_IM2D_NS_BEGIN


class IAreaEdgeData
{
public:
    virtual ~IAreaEdgeData()
    {
        //  nothing.
    }

    virtual int getSize() = 0;
    virtual double getStepSize() = 0;
    virtual void setP0(int index, double value) = 0;
    virtual double getP0(int index) = 0;
    virtual double getP1(int index) = 0;
    virtual void setQ0(int index, double value) = 0;
    virtual double getQ0(int index) = 0;
    virtual double getQ1(int index) = 0;
    virtual void setC0(int index, double value) = 0;
    virtual double getC0(int index) = 0;
    virtual double getC1(int index) = 0;

};

BIO_SLV_FD_IM2D_NS_END

#endif
