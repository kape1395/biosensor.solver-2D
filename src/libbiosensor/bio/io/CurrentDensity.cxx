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
#include "CurrentDensity.hxx"
#include "../Exception.hxx"
#include <iostream>

/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::CurrentDensity::CurrentDensity(
    std::string& name,
    BIO_SLV_NS::ISolver* solver,
    BIO_IO_NS::IContext* Context
)
{
    this->name = name;
    this->solver = solver;
    this->Context = Context;
    this->output = 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::CurrentDensity::~CurrentDensity()
{
    if (output)
        Context->close(output);
    output = 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_IO_NS::CurrentDensity::solveEventOccured()
{
    if (!output)
    {
        output = Context->getOutputStream(name);
        output->precision(12);
        (*output) << "# Time\tStep\tCurrentDensity" << std::endl;
    }

    BIO_SLV_NS::IIterativeSolver* iterativeSolver = dynamic_cast<BIO_SLV_NS::IIterativeSolver*>(solver);
    if (iterativeSolver)
    {
        (*output) << iterativeSolver->getSolvedTime() << '\t' << iterativeSolver->getSolvedIterationCount();
    }
    else
    {
        (*output) << '\t';
    }

    (*output) << "\t" << solver->getTransducer()->getOutput() << std::endl;
}


/* ************************************************************************** */
/* ************************************************************************** */
