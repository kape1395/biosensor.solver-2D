#ifndef BIO_DM_IComposite1DArea_HXX
#define BIO_DM_IComposite1DArea_HXX
#include "../../biosensor.hxx"
#include "IGrid1D.hxx"
BIO_DM_NS_BEGIN
class IComposite1DBound;


/**
 *  Composite data model.
 */
class IComposite1DArea : public IGrid1D
{
public:
    virtual IComposite1DBound* getNextBound() = 0;
    virtual IComposite1DBound* getPrevBound() = 0;
};


BIO_DM_NS_END
#endif
