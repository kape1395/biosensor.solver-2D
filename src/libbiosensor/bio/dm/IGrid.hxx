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
#ifndef BIO_DM_IGrid_HXX
#define BIO_DM_IGrid_HXX
#include "../../biosensor.hxx"
#include "IDataModel.hxx"
#include <biosensor-xml.hxx>

BIO_DM_NS_BEGIN


/**
 *  Base interface for all grid-based data models, such as finite differences
 *  data model. This interface is intented to be implemented by IDataModel
 *  implementations.
 */
class IGrid
{
public:
    virtual ~IGrid()
    {
        //  Empty virtual destructor.
    }

    virtual int getSubstanceCount() = 0;
    virtual BIO_XML_NS::model::Substance* getSubstanceConf(int index) = 0;
};



BIO_DM_NS_END

#endif
