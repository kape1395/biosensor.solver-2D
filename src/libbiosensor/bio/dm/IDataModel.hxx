#ifndef BIO_DM_IDataModel_HXX
#define BIO_DM_IDataModel_HXX
#include "../../biosensor.hxx"

BIO_DM_NS_BEGIN


/**
 *  Just empty base interface for all data models.
 */
class IDataModel
{
public:

    virtual ~IDataModel()
    {
        //  Empty virtual destructor.
    }

};



BIO_DM_NS_END

#endif
