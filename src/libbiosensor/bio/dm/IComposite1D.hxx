#ifndef BIO_DM_IComposite1D_HXX
#define BIO_DM_IComposite1D_HXX
#include "../../biosensor.hxx"
#include "IDataModel.hxx"
BIO_DM_NS_BEGIN
class IComposite1DArea;
class IComposite1DBound;


/**
 *  Composite data model.
 */
class IComposite1D : public IDataModel
{
public:
    virtual int getPartCount() = 0;
    virtual IComposite1DArea* getArea(int i) = 0;
    virtual IComposite1DBound* getBound(int i) = 0;
};


BIO_DM_NS_END
#endif
