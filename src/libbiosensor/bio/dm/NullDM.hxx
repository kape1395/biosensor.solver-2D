#ifndef BIO_DM_NullDM_HXX
#define BIO_DM_NullDM_HXX
#include "../../biosensor.hxx"
#include "IDataModel.hxx"
BIO_DM_NS_BEGIN


/**
 *  Empty data model.
 *  TODO: Maybe we have to delete this type???
 */
class NullDM : public IDataModel
{
public:
    NullDM();
    virtual ~NullDM();

    /**
     *  Copies the state from source to this data model.
     */
    virtual void setState(IDataModel *source);
};



BIO_DM_NS_END
#endif
