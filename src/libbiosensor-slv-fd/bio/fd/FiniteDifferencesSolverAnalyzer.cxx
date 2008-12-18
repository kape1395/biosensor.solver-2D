#include "FiniteDifferencesSolverAnalyzer.hxx"
#include <bio/Exception.hxx>
#include <vector>


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  NOTE: There is a litle of code duplication.
 *  NOTE: For now only ConstantAxisPart elements are supported.
 */
void BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer::analyze(
    BIO_XML_NS::model::Model* config
)
{
    using BIO_XML_NS::model::Symbol;
    using BIO_XML_NS::model::solver::FiniteDifferences;
    using BIO_XML_NS::model::solver::Axis;

    typedef std::vector<Symbol*>::iterator Symbol_it;
    typedef FiniteDifferences::axis_iterator Axis_it;

    LOG4CXX_INFO(log, "cleanup...");

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
    //
    //  Release all old data.
    ////////////////////////////////////////////////////////////////////////////

    LOG4CXX_INFO(log, "cleanup... Done");


    this->config = config;
    this->structureAnalyzer.analyze(config);

    //  If 0 is passed as config, only cleanup should be performed.
    if (config == 0)
        return;


    LOG4CXX_INFO(log, "analyze...");


    FiniteDifferences* fdSolver = dynamic_cast<FiniteDifferences*>(&config->solver());
    if (!fdSolver)
    {
        LOG4CXX_ERROR(log, "Received configuration with non FiniteDifferences solver. I dont know what to do with it.");
        throw Exception("Invalid solver spec.");
    }


    ////////////////////////////////////////////////////////////////////////////
    //  Collect axis-parts and assign them to the concrete "cells"
    //

    axisPartsH = new Axis*[structureAnalyzer.getPointsH().size() - 1];    // intervalu yra 1 maziau nei tasku.
    axisPartsV = new Axis*[structureAnalyzer.getPointsV().size() - 1];

    for (int i = 0; i < structureAnalyzer.getPointsH().size() - 1; axisPartsH[i++] = 0);
    for (int i = 0; i < structureAnalyzer.getPointsV().size() - 1; axisPartsV[i++] = 0);

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
            }
        }
    }

    ////////////    Check if all intervals were covered in the config.
    int unspecifiedIntervals = 0;
    for (int i = 0; i < structureAnalyzer.getPointsH().size() - 1; i++)
    {
        if (axisPartsH[i] == 0)
            unspecifiedIntervals++;
    }
    for (int i = 0; i < structureAnalyzer.getPointsV().size() - 1; i++)
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

    LOG4CXX_INFO(log, "analyze... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
