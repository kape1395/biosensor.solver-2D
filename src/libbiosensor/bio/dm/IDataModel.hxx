#ifndef BIO_DM_IDataModel_HXX
#define BIO_DM_IDataModel_HXX
#include "../../biosensor.hxx"

BIO_DM_NS_BEGIN


/**
 *  Just base interface for all data models.
 */
class IDataModel
{
public:

    /**
     *  Copies the state from source to this data model.
     */
    virtual void setState(IDataModel *source) = 0;

    /**
     *  Destructor.
     */
    virtual ~IDataModel()
    {
        //  Empty virtual destructor.
    }

};



BIO_DM_NS_END

#endif
