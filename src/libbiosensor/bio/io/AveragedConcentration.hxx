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
#ifndef BIO_IO_AveragedConcentration_HXX
#define BIO_IO_AveragedConcentration_HXX
#include "../../biosensor.hxx"
#include "../cfg/StructureAnalyzer.hxx"
#include "../slv/ISolver.hxx"
#include "../trd/IntegralOverArea.hxx"
#include "IOutput.hxx"
#include "IContext.hxx"
#include <biosensor-xml.hxx>
#include <string>
#include <ostream>
#include <vector>
BIO_IO_NS_BEGIN


/**
 *  An output generator for an averaged concentration.
 *  Can generate output for specific medium or for whole biosensor.
 *
 *  \author k.petrauskas
 */
class AveragedConcentration : public BIO_IO_NS::IOutput
{
private:
    typedef std::vector<BIO_TRD_NS::IntegralOverArea*> Integrals;
    typedef std::vector<BIO_XML_MODEL_NS::Substance*> Substances;
    std::string name;
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer;
    BIO_SLV_NS::ISolver* solver;
    BIO_IO_NS::IContext* context;
    BIO_XML_MODEL_NS::MediumName* medium;

    std::ostream* output;

    Substances substances;
    Integrals integrals;

public:
    /**
     *  Constructor.
     */
    AveragedConcentration(
        std::string& name,
        BIO_SLV_NS::ISolver* solver,
        BIO_IO_NS::IContext* context,
        BIO_XML_MODEL_NS::MediumName* medium
    );

    /**
     *  Destructor.
     */
    virtual ~AveragedConcentration();

    /**
     *  EventListener.
     */
    virtual void solveEventOccured();

    /**
     *  Reset listener's internal state.
     */
    virtual void reset()
    {
        //  Nothing to reset.
    }

};



BIO_IO_NS_END
#endif
