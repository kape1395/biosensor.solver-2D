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
#ifndef BIO_TRD_AmperometricInjectedElectrode2D_HXX
#define BIO_TRD_AmperometricInjectedElectrode2D_HXX
#include "../../biosensor.hxx"
#include "../slv/ISolver.hxx"
#include "../slv/ITransducer.hxx"
#include "../cfg/BoundAnalyzer.hxx"
#include "../dm/IDataModel.hxx"
#include "../dm/IGrid2D.hxx"
#include "../dm/ICursor2D.hxx"
#include "../dm/IComposite2D.hxx"
#include "../dm/ISegmentSplit.hxx"
#include "IntegralOverArea.hxx"
#include <biosensor-xml.hxx>
#include <string>
#include <vector>
BIO_TRD_NS_BEGIN


/**
 *  Transducer: InjectedElectrode.
 */
class AmperometricInjectedElectrode2D : public BIO_SLV_NS::ITransducer
{
private:
    static const double CONST_n_e  = 2.0;
    static const double CONST_F    = 96485.0;

private:
    BIO_SLV_NS::ISolver* solver;
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer;
    BIO_DM_NS::IComposite2D* dataModel;
    BIO_XML_MODEL_NS::MediumName mediumName;
    BIO_XML_MODEL_NS::ReactionName reactionName;

    int substanceIndexes[];

    std::vector<BIO_DM_NS::IGrid2D*> areas;

    double calculatedOutput;
    long   calculatedOutputForStep;

    IntegralOverArea* areaIntegrator;

public:
    AmperometricInjectedElectrode2D(
        BIO_SLV_NS::ISolver* solver,
        BIO_XML_MODEL_NS::MediumName& mediumName,
        BIO_XML_MODEL_NS::ReactionName& reactionName
    );

    virtual ~AmperometricInjectedElectrode2D();

    /**
     *  Returns current density, generated by the biosensor.
     */
    virtual double getOutput();

};



BIO_TRD_NS_END
#endif
