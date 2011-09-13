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
#ifndef BIO_SLV_StopIfInvalidConcentrations_HXX
#define BIO_SLV_StopIfInvalidConcentrations_HXX
#include "../../biosensor.hxx"
#include "ISolverListener.hxx"
#include "../dm/IComposite2D.hxx"

BIO_SLV_NS_BEGIN


/**
 *  Stops calculations, if NaN`s or negative concentrations are found in the
 *  solver`s data.
 */
class StopIfInvalidConcentrations : public ISolverListener
{
private:

    BIO_SLV_NS::IIterativeSolver* iterativeSolver;
    BIO_DM_NS::IComposite2D* dataModel;

    long checkEveryNumberOfSteps;
    long nextStepForCheck;

public:
    /**
     *  Constructor.
     */
    StopIfInvalidConcentrations(
        ISolver* solver,
        long checkEveryNumberOfSteps = 100
    );

    /**
     *  Destructor.
     */
    virtual ~StopIfInvalidConcentrations();

    /**
     *  EventListener.
     */
    virtual void solveEventOccured();

    /**
     *  Reset listener's internal state.
     */
    virtual void reset();

protected:

    /**
     *  Check one sub-area for the data validity.
     */
    virtual bool checkSubArea(BIO_DM_NS::IGrid2D* area, int posH, int posV);

};



BIO_SLV_NS_END
#endif
