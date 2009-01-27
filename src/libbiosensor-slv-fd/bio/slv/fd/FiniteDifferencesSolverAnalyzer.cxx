#include "FiniteDifferencesSolverAnalyzer.hxx"
#include <bio/Exception.hxx>
#include <bio/dm/ConstantSegmentSplit.hxx>
#include <vector>


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  NOTE: There is a litle of code duplication.
 *  NOTE: For now only ConstantAxisPart elements are supported.
 */
BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer::FiniteDifferencesSolverAnalyzer(
    BIO_XML_NS::model::Model* config
) :
        log(log4cxx::Logger::getLogger("libbiosensor-slv-fd::FiniteDifferencesSolverAnalyzer")),
        structureAnalyzer(config)
{
    this->config = 0;
    this->axisPartsH = 0;
    this->axisPartsV = 0;

    using BIO_XML_NS::model::Symbol;
    using BIO_XML_NS::model::solver::FiniteDifferences;
    using BIO_XML_NS::model::solver::Axis;

    typedef std::vector<Symbol*>::iterator Symbol_it;
    typedef FiniteDifferences::axis_iterator Axis_it;

    LOG4CXX_INFO(log, "FiniteDifferencesSolverAnalyzer()...");

    this->config = config;

    FiniteDifferences* fdSolver = dynamic_cast<FiniteDifferences*>(&config->solver());
    if (!fdSolver)
    {
        LOG4CXX_ERROR(log, "Received configuration with non FiniteDifferences solver. I dont know what to do with it.");
        throw Exception("Invalid solver spec.");
    }


    ////////////////////////////////////////////////////////////////////////////
    //  Collect axis-parts and assign them to the concrete "cells"
    //
    partCountH = structureAnalyzer.getPointsH().size() - 1; // intervalu yra 1 maziau nei tasku.
    partCountV = structureAnalyzer.getPointsV().size() - 1; // intervalu yra 1 maziau nei tasku.

    axisPartsH = new Axis*[partCountH];
    axisPartsV = new Axis*[partCountV];
    axisPartSegmentSplitH = new BIO_DM_NS::ISegmentSplit*[partCountH];
    axisPartSegmentSplitV = new BIO_DM_NS::ISegmentSplit*[partCountV];

    for (int i = 0; i < partCountH; i++)
    {
        axisPartsH[i] = 0;
        axisPartSegmentSplitH[i] = 0;
    }
    for (int i = 0; i < partCountV; i++)
    {
        axisPartsV[i] = 0;
        axisPartSegmentSplitV[i] = 0;
    }

    std::vector<Symbol*>* symbols;


    ////////////    Analyze HORIZONTAL axis
    symbols = &structureAnalyzer.getPointsH();
    int pointPosition = 0;
    for (Symbol_it point = symbols->begin(); point < symbols->end() - 1; point++, pointPosition++)
    {
        for (Axis_it axis = fdSolver->axis().begin(); axis < fdSolver->axis().end(); axis++)
        {
            if (axis->from() != (*point)->name())
                continue;   //  If this is not a needed axis - skip it.

            if (axis->to() != (*(point+1))->name())
            {
                LOG4CXX_ERROR(log, "In solver/axis \"from\" and \"to\" must be subsequent points in the corresponding axis.");
                throw Exception("Invalid solver spec.");
            }

            axisPartsH[pointPosition] = &*axis;
            axisPartSegmentSplitH[pointPosition] = this->createSegmentSplit(&*axis);
        }
    }


    ////////////    Analyze VERTICAL axis
    if (!structureAnalyzer.isTwoDimensional())
    {
        axisPartsV[0] = 0;  //  No steps is in this case (only one point)
    }
    else
    {
        symbols = &structureAnalyzer.getPointsV();
        pointPosition = 0;
        for (Symbol_it point = symbols->begin(); point < symbols->end() - 1; point++, pointPosition++)
        {
            for (Axis_it axis = fdSolver->axis().begin(); axis < fdSolver->axis().end(); axis++)
            {
                if (axis->from() != (*point)->name())
                    continue;   //  If this is not a needed axis - skip it.

                if (axis->to() != (*(point+1))->name())
                {
                    LOG4CXX_ERROR(log, "In solver/axis \"from\" and \"to\" must be subsequent points in the corresponding axis.");
                    throw Exception("Invalid solver spec.");
                }

                axisPartsV[pointPosition] = &*axis;
                axisPartSegmentSplitV[pointPosition] = this->createSegmentSplit(&*axis);
            }
        }
    }

    ////////////    Check if all intervals were covered in the config.
    int unspecifiedIntervals = 0;
    for (int i = 0; i < partCountH; i++)
    {
        if (axisPartsH[i] == 0)
            unspecifiedIntervals++;
    }
    for (int i = 0; i < partCountV; i++)
    {
        if (axisPartsV[i] == 0)
            unspecifiedIntervals++;
    }
    if (unspecifiedIntervals)
    {
        LOG4CXX_ERROR(log, "Not all axis parts are specified by solver/axis elements");
        throw Exception("Invalid solver spec.");
    }

    //
    //  Collect axis-parts and assign them to the concrete "cells"
    ////////////////////////////////////////////////////////////////////////////

    this->timeStep = fdSolver->timeStep();

    LOG4CXX_INFO(log, "FiniteDifferencesSolverAnalyzer()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer::~FiniteDifferencesSolverAnalyzer()
{
    LOG4CXX_INFO(log, "~FiniteDifferencesSolverAnalyzer()...");

    ////////////////////////////////////////////////////////////////////////////
    //  Release all old data.
    //
    if (axisPartsH)
    {
        delete[] axisPartsH;
        axisPartsH = 0;
    }
    if (axisPartsV)
    {
        delete[] axisPartsV;
        axisPartsV = 0;
    }

    for (int i = 0; i < partCountH; i++)
    {
        if (axisPartSegmentSplitH[i])
            delete axisPartSegmentSplitH[i];
    }
    delete [] axisPartSegmentSplitH;
    axisPartSegmentSplitH = 0;

    for (int i = 0; i < partCountV; i++)
    {
        if (axisPartSegmentSplitV[i])
            delete axisPartSegmentSplitV[i];
    }
    delete [] axisPartSegmentSplitV;
    axisPartSegmentSplitV = 0;


    //
    //  Release all old data.
    ////////////////////////////////////////////////////////////////////////////

    LOG4CXX_INFO(log, "~FiniteDifferencesSolverAnalyzer()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_DM_NS::ISegmentSplit* BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer::createSegmentSplit(
    BIO_XML_NS::model::solver::Axis* axis
)
{
    if (axis == 0)
        return 0;

    if (dynamic_cast<BIO_XML_NS::model::solver::ConstantAxisPart*>(axis) != 0)
    {
        BIO_XML_NS::model::solver::ConstantAxisPart* cap;
        cap = dynamic_cast<BIO_XML_NS::model::solver::ConstantAxisPart*>(axis);

        double from = structureAnalyzer.getSymbol(axis->from())->value();
        double to = structureAnalyzer.getSymbol(axis->to())->value();
        int count = cap->stepCount();

        BIO_DM_NS::ConstantSegmentSplit* split = new BIO_DM_NS::ConstantSegmentSplit(
            from,
            to - from,
            count
        );
        return split;
    }
    else
    {
        throw Exception("Used subclass of bio::model::solver::Axis is not supported yet.");
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
