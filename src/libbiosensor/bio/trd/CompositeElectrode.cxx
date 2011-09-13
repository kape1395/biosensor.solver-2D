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
#include "CompositeElectrode.hxx"
#include "../Logging.hxx"
#include "../Exception.hxx"
#define LOGGER "libbiosensor::CompositeElectrode: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::CompositeElectrode::CompositeElectrode()
{
    //  Nothing to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::CompositeElectrode::~CompositeElectrode()
{
    std::vector<BIO_SLV_NS::ITransducer*>::iterator it;
    for (it = transducersToDelete.begin(); it < transducersToDelete.end(); it++)
    {
        delete *it;
    }
    transducers.clear();
    transducersToDelete.clear();
}

/* ************************************************************************** */
/* ************************************************************************** */
void BIO_TRD_NS::CompositeElectrode::addTransducer(BIO_SLV_NS::ITransducer* transducer, bool deleteOnDestruction)
{
    transducers.push_back(transducer);
    if (deleteOnDestruction)
    {
        transducersToDelete.push_back(transducer);
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_TRD_NS::CompositeElectrode::getOutput()
{
    double result = 0.0;
    std::vector<BIO_SLV_NS::ITransducer*>::iterator it;
    for (it = transducersToDelete.begin(); it < transducersToDelete.end(); it++)
    {
        result += (*it)->getOutput();
    }
    return result;
}


/* ************************************************************************** */
/* ************************************************************************** */
