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

    if ((this->grid = dynamic_cast<BIO_DM_NS::IGrid2D*>(solver->getData())) == 0)
    {
        throw Exception("ConcentrationProfile: IGrid2D DataModel is required");
    }
    this->cursor = this->grid->newGridCursor();
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::ConcentrationProfile::~ConcentrationProfile()
{
    delete cursor;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_IO_NS::ConcentrationProfile::solveEventOccured()
{
    using BIO_SLV_NS::IIterativeSolver;
    int substCount = grid->getSubstanceCount();

    currentIndex++;

    std::ostream* out = indexed
                        ? context->getOutputStream(name, currentIndex, overwrite)
                        : context->getOutputStream(name, overwrite);


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

    haveLastOutput = true;
    context->close(out);
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
