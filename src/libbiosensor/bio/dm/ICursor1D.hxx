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
#ifndef BIO_DM_ICursor1D_HXX
#define BIO_DM_ICursor1D_HXX
#include "../../biosensor.hxx"
#include "IConcentrations.hxx"
BIO_DM_NS_BEGIN


/**
 * 1D Cursor. It is used to navigate in 1D grid data model.
 */
class ICursor1D
{
public:
    virtual ~ICursor1D()
    {
        //  Empty virtual destructor.
    }

    virtual void prev() = 0;
    virtual void next() = 0;
    virtual void start() = 0;
    virtual void end() = 0;
    virtual bool isValid() = 0;
    virtual IConcentrations* getConcentrations() = 0;
};



BIO_DM_NS_END

#endif
