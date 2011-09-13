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
#ifndef BIO_DM_IConcentrations_HXX
#define BIO_DM_IConcentrations_HXX
#include "../../biosensor.hxx"
BIO_DM_NS_BEGIN


/**
 *  Interface to access concentration values.
 */
class IConcentrations
{
public:

    virtual ~IConcentrations()
    {
        //  Empty virtual destructor.
    }

    /**
     *  Returns concentration of the substance with index <code>substanceNr</code>.
     *  @param substanceNr  Substance index.
     *  @return Concentration for the substance.
     */
    virtual double getConcentration(int substanceNr) = 0;

    /**
     *  Sets new concentration for the substance with specified index.
     *  @param substanceNr      Substance index.
     *  @param concentration    New concentration for the substance.
     */
    virtual void setConcentration(int substanceNr, double concentration) = 0;

};

BIO_DM_NS_END
#endif
