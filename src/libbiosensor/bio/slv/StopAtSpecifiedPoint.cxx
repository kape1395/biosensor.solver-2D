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
#include "StopAtSpecifiedPoint.hxx"
#include "../Exception.hxx"
#include "../Logging.hxx"
#define LOGGER "libbiosensor::StopAtSpecifiedPoint: "

/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::StopAtSpecifiedPoint::StopAtSpecifiedPoint(
    ISolver* solver
)
{
    this->solver = dynamic_cast<IIterativeSolver*>(solver);
    this->stepCount = 0;
    this->time = 0.0;

    if (this->solver == 0)
        throw Exception("StopAtSpecifiedPoint: Solver must implement IIterativeSolver");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::StopAtSpecifiedPoint::~StopAtSpecifiedPoint()
{
    //  Nothing to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::StopAtSpecifiedPoint::solveEventOccured()
{
    if (!solver->isStopped() && (
                (stepCount != 0 && solver->getSolvedIterationCount() >= stepCount) ||
                (time      != 0 && solver->getSolvedTime()           >= time     )
            ))
    {
        solver->stop();
        LOG_INFO(LOGGER << "Solver is stopped by request at a specified time or stepCount.");
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
