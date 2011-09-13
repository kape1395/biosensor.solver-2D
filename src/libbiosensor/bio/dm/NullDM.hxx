/*
 * Copyright 2011 Karolis Petrauskas
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
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
