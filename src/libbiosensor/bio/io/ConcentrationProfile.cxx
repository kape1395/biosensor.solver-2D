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
#include "ConcentrationProfile.hxx"
#include "IContext.hxx"
#include "../Exception.hxx"
#include "../dm/ISegmentSplit.hxx"
#include "../Logging.hxx"
#include <iostream>
#include <sstream>
#include <cmath>
#define LOGGER "libbiosensor::ConcentrationProfile: "

/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::ConcentrationProfile::ConcentrationProfile(
    std::string& name,
    BIO_SLV_NS::ISolver* solver,
    BIO_IO_NS::IContext* context
)
{
    this->name = name;
    this->indexed = false;
    this->haveLastOutput = false;
    this->overwrite = false;
    this->currentIndex = -1;
    this->solver = solver;
    this->context = context;
    this->precision = -1;

    this->lastStateReader = 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::ConcentrationProfile::~ConcentrationProfile()
{
    if (lastStateReader)
    {
        delete lastStateReader;
        lastStateReader = 0;
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_IO_NS::ConcentrationProfile::solveEventOccured()
{
    setSolverState(solver->getState(), overwrite);
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_IO_NS::ConcentrationProfile::reset()
{
    currentIndex = -1;
    haveLastOutput = false;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_IO_NS::ConcentrationProfile::setRepeatable(bool repeatable)
{
    indexed = repeatable;
    reset();
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_IO_NS::ConcentrationProfile::setOverwrite(bool overwrite)
{
    this->overwrite = overwrite;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_IO_NS::ConcentrationProfile::setPrecision(int precision)
{
    this->precision = precision;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::ISolverState* BIO_IO_NS::ConcentrationProfile::getSolverState()
{
    if (lastStateReader)
    {
        delete lastStateReader;
    }
    lastStateReader = createReaderForLastOutput();
    return lastStateReader;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_IO_NS::ConcentrationProfile::setSolverState(BIO_SLV_NS::ISolverState* state)
{
    setSolverState(state, true);
}

/* ************************************************************************** */
/* ************************************************************************** */
void BIO_IO_NS::ConcentrationProfile::setSolverState(BIO_SLV_NS::ISolverState* state, bool overwrite)
{
    using BIO_SLV_NS::IIterativeSolver;

    BIO_DM_NS::IGrid2D* grid;
    BIO_DM_NS::ICursor2D* cursor;
    if ((grid = dynamic_cast<BIO_DM_NS::IGrid2D*>(state->getData())) == 0)
    {
        throw Exception("ConcentrationProfile: IGrid2D DataModel is required");
    }
    cursor = grid->newGridCursor();

    int substCount = grid->getSubstanceCount();

    currentIndex++;

    std::ostream* out = indexed
                        ? context->getOutputStream(name, currentIndex, overwrite)
                        : context->getOutputStream(name, overwrite);

    if (precision != -1)
    {
        out->precision(precision);
    }

    IIterativeSolver* iterativeSolver = dynamic_cast<IIterativeSolver*>(solver);
    if (iterativeSolver != 0)
    {
        (*out) << "#"
        << " SolvedIterationCount=" << iterativeSolver->getSolvedIterationCount()
        << " SolvedTime="           << iterativeSolver->getSolvedTime()
        << std::endl;
    }
    else
    {
        (*out) << "# SolvedIterationCount=? SolvedTime=?" << std::endl;
    }

    std::stringstream header;
    header << "# pos_h\tpos_v\tidx_h\tidx_v";
    for (int s = 0; s < substCount; s++)
    {
        header << '\t' << grid->getSubstanceConf(s)->name();
    }

    int h;
    int v;
    cursor->colStart();
    cursor->rowStart();
    for (v = 0; cursor->isValid(); v++, cursor->down())
    {
        (*out) << std::endl << header.str() << std::endl;
        for (h = 0; cursor->isValid(); h++, cursor->right())
        {
            BIO_DM_NS::IConcentrations* concentrations = cursor->getConcentrations();

            (*out)
            << grid->getPointPositionsH()->getPointPosition(h) << '\t'
            << grid->getPointPositionsV()->getPointPosition(v) << '\t'
            << h << '\t' << v
            ;
            for (int s = 0; s < substCount; s++)
            {
                (*out) << '\t' << concentrations->getConcentration(s);
            }
            (*out) << std::endl;
        }
        cursor->rowStart();
    }
    delete cursor;

    haveLastOutput = true;
    context->close(out);
}


/* ************************************************************************** */
/* ************************************************************************** */
bool BIO_IO_NS::ConcentrationProfile::hasSolverState()
{
    return haveLastOutput;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::ConcentrationProfileReader* BIO_IO_NS::ConcentrationProfile::createReaderForLastOutput()
{
    using BIO_IO_NS::ConcentrationProfileReader;

    if (!haveLastOutput)
    {
        LOG_WARN(LOGGER
                 << "createReaderForLastOutput: "
                 << "Reader is requested but no output was done before. Returning null."
                );
        return 0;
    }

    std::istream* input = indexed
                          ? context->getInputStream(name, indexed)
                          : context->getInputStream(name);

    ConcentrationProfileReader* reader = new ConcentrationProfileReader(
        solver->getConfig(),
        *input
    );

    context->close(input);
    return reader;
}

/* ************************************************************************** */
/* ************************************************************************** */
