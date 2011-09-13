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
#include "StopIfConcentrationsOscillateBySpace.hxx"
#include "../Exception.hxx"
#include "../Logging.hxx"
#include <iostream>
#include <cmath>
#include <memory>
#define LOGGER "libbiosensor::StopIfConcentrationsOscillateBySpace: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::StopIfConcentrationsOscillateBySpace::StopIfConcentrationsOscillateBySpace(
    ISolver* solver,
    long checkEveryNumberOfSteps
) : StopIfInvalidConcentrations(solver, checkEveryNumberOfSteps)
{
    //  Nothing to do here
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::StopIfConcentrationsOscillateBySpace::~StopIfConcentrationsOscillateBySpace()
{
    //  Nothing to do here
}


/* ************************************************************************** */
/* ************************************************************************** */
bool BIO_SLV_NS::StopIfConcentrationsOscillateBySpace::checkSubArea(
    BIO_DM_NS::IGrid2D* area,
    int areaPosH, int areaPosV)
{
    std::auto_ptr<BIO_DM_NS::ICursor2D> cursor(area->newGridCursor());

    int substCount = area->getSubstanceCount();

    int      pointsLenght = 5;
    int      pointsHead = 0;
    double** points = new double*[substCount];

    int      derivatesLenght = 3;
    int      derivatesHead = 0;
    double** derivates = new double*[substCount];

    for (int s = 0; s < substCount; s++)
    {
        points[s] = new double[pointsLenght];
        derivates[s] = new double[derivatesLenght];
    }


    int h;
    int v;
    bool oscillation = false;

    //
    //  Horizontal derivates
    //
    for (cursor->colStart(), v = 0; cursor->rowStart(), !oscillation && cursor->isValid(); cursor->down(), v++)
    {
        pointsHead = 0;
        derivatesHead = 0;
        for (h = 0 ; !oscillation && cursor->isValid(); cursor->right(), h++)
        {
            int pointsIdx0 = (pointsHead + pointsLenght - 0) % pointsLenght;    // == pointsHead
            int pointsIdx1 = (pointsHead + pointsLenght - 1) % pointsLenght;
            int pointsIdx2 = (pointsHead + pointsLenght - 2) % pointsLenght;

            int derivatesIdx0 = (derivatesHead + derivatesLenght - 0) % derivatesLenght;    // == derivatesHead
            int derivatesIdx1 = (derivatesHead + derivatesLenght - 1) % derivatesLenght;
            int derivatesIdx2 = (derivatesHead + derivatesLenght - 2) % derivatesLenght;

            BIO_DM_NS::IConcentrations* concentrations = cursor->getConcentrations();
            for (int s = 0; !oscillation && (s < area->getSubstanceCount()); s++)
            {
                points[s][pointsIdx0] = concentrations->getConcentration(s);

                if (h >= 3)
                {
                    //  Calculate second derivate. Assuming fixed step.
                    //  Only sign is interesting.
                    derivates[s][derivatesIdx0] = points[s][pointsIdx2] + points[s][pointsIdx0]
                                                  - (2.0 * points[s][pointsIdx1]);
                }

                if (h >= 5)
                {
                    oscillation = oscillation || (
                                      derivates[s][derivatesIdx0] >= 0 &&
                                      derivates[s][derivatesIdx2] >= 0 &&
                                      derivates[s][derivatesIdx1] < 0
                                  ) || (
                                      derivates[s][derivatesIdx0] <= 0 &&
                                      derivates[s][derivatesIdx2] <= 0 &&
                                      derivates[s][derivatesIdx1] > 0
                                  );
                }

            }
            pointsHead    = (pointsHead    + 1) % pointsLenght;
            derivatesHead = (derivatesHead + 1) % derivatesLenght;
        }
    }

    //
    //  Vertical derivates
    //
    for (cursor->rowStart(), h = 0; cursor->colStart(), !oscillation && cursor->isValid(); cursor->right(), h++)
    {
        pointsHead = 0;
        derivatesHead = 0;
        for (v = 0 ; !oscillation && cursor->isValid(); cursor->down(), v++)
        {
            int pointsIdx0 = (pointsHead + pointsLenght - 0) % pointsLenght;    // == pointsHead
            int pointsIdx1 = (pointsHead + pointsLenght - 1) % pointsLenght;
            int pointsIdx2 = (pointsHead + pointsLenght - 2) % pointsLenght;

            int derivatesIdx0 = (derivatesHead + derivatesLenght - 0) % derivatesLenght;    // == derivatesHead
            int derivatesIdx1 = (derivatesHead + derivatesLenght - 1) % derivatesLenght;
            int derivatesIdx2 = (derivatesHead + derivatesLenght - 2) % derivatesLenght;

            BIO_DM_NS::IConcentrations* concentrations = cursor->getConcentrations();
            for (int s = 0; !oscillation && (s < area->getSubstanceCount()); s++)
            {
                points[s][pointsIdx0] = concentrations->getConcentration(s);

                if (v >= 3)
                {
                    //  Calculate second derivate. Assuming fixed step.
                    //  Only sign is interesting.
                    derivates[s][derivatesIdx0] = points[s][pointsIdx2] + points[s][pointsIdx0]
                                                  - (2.0 * points[s][pointsIdx1]);
                }

                if (v >= 5)
                {
                    oscillation = oscillation || (
                                      derivates[s][derivatesIdx0] >= 0 &&
                                      derivates[s][derivatesIdx2] >= 0 &&
                                      derivates[s][derivatesIdx1] < 0
                                  ) || (
                                      derivates[s][derivatesIdx0] <= 0 &&
                                      derivates[s][derivatesIdx2] <= 0 &&
                                      derivates[s][derivatesIdx1] > 0
                                  );
                }

            }
            pointsHead    = (pointsHead    + 1) % pointsLenght;
            derivatesHead = (derivatesHead + 1) % derivatesLenght;
        }
    }

    if (oscillation)
    {
        LOG_WARN(LOGGER
                 << "An oscillation of the concentration found for the"
                 << " substance in the subArea with position:"
                 << " h=" << areaPosH << " v=" << areaPosV
                );
        return false;
    }

    for (int s = 0; s < substCount; s++)
    {
        delete[] points[s];
        delete[] derivates[s];
    }
    delete[] points;
    delete[] derivates;

    return true;
}


/* ************************************************************************** */
/* ************************************************************************** */
