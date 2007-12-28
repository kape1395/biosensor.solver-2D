#ifndef BIO_DM_IComposite1DBound_HXX
#define BIO_DM_IComposite1DBound_HXX
#include "../../biosensor.hxx"
BIO_DM_NS_BEGIN
// Used classes (wf-decl instead of include)
class IComposite1DArea;
class IConcentrations;


/**
 *  Composite data model.
 */
class IComposite1DBound
{
public:
    virtual IComposite1DArea* getNextArea() = 0;
    virtual IComposite1DArea* getPrevArea() = 0;
    virtual IConcentrations* getConcentrations() = 0;
};


BIO_DM_NS_END
#endif
