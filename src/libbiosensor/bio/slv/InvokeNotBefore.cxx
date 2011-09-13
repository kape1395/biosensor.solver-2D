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
#include "InvokeNotBefore.hxx"
#include "../Exception.hxx"

/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::InvokeNotBefore::InvokeNotBefore(
    ISolver* solver
)
{
    this->solver = dynamic_cast<IIterativeSolver*>(solver);
    this->stepCount = 0;
    this->time = 0.0;

    if (this->solver == 0)
        throw Exception("InvokeNotBefore: Solver must implement IIterativeSolver");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::InvokeNotBefore::~InvokeNotBefore()
{
    for (SLVector::iterator l = listenersToDelete.begin(); l < listenersToDelete.end(); l++)
    {
        delete *l;
    }
    listeners.clear();
    listenersToDelete.clear();
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::InvokeNotBefore::addListener(
    BIO_SLV_NS::ISolverListener* listener,
    bool deleteAtDestruction
)
{
    listeners.push_back(listener);
    if (deleteAtDestruction)
    {
        listenersToDelete.push_back(listener);
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::InvokeNotBefore::solveEventOccured()
{
    if (    (stepCount != 0 && solver->getSolvedIterationCount() >= stepCount) ||
            (time      != 0 && solver->getSolvedTime()           >= time     )
       )
    {
        for (SLVector::iterator l = listeners.begin(); l < listeners.end(); l++)
        {
            (*l)->solveEventOccured();
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::InvokeNotBefore::reset()
{
    for (SLVector::iterator l = listeners.begin(); l < listeners.end(); l++)
        (*l)->reset();
}


/* ************************************************************************** */
/* ************************************************************************** */
