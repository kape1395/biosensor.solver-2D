#ifndef BIO_SLV_FD_IM2D_IAreaEdgeFunction_HXX
#define BIO_SLV_FD_IM2D_IAreaEdgeFunction_HXX
#include "../../../../biosensor-slv-fd.hxx"

BIO_SLV_FD_IM2D_NS_BEGIN


/**
 *  It's an interface used to provifr functions for the bound conditions.
 */
class IAreaEdgeFunction
{
public:
    virtual ~IAreaEdgeFunction()
    {
        //  nothing.
    }

    /**
     *  Get "length" of this function. I.e. #getValue will accept
     *  indexes from range [0..size).
     */
    virtual int getSize() = 0;

    /**
     *  Returns a function value at the particular position
     *  specified by the index.
     */
    virtual double getValue(int index) = 0;

};

BIO_SLV_FD_IM2D_NS_END

#endif
