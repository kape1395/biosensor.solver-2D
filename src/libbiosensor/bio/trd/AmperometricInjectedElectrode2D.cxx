#include "AmperometricInjectedElectrode2D.hxx"
#include "../Logging.hxx"
#include "../Exception.hxx"
#include "../dm/ConstantSegmentSplit.hxx"
#include "../slv/IIterativeSolver.hxx"
#define LOGGER "libbiosensor::AmperometricInjectedElectrode2D: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::AmperometricInjectedElectrode2D::AmperometricInjectedElectrode2D(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::MediumName& mediumName,
    BIO_XML_MODEL_NS::SubstanceName& substanceName,
    BIO_XML_MODEL_NS::SymbolName& reactionSpeedSymbolName
)
{
    if (!dynamic_cast<BIO_SLV_NS::IIterativeSolver*>(solver))
        throw Exception("InjectedElectrode: Solver must implement IIterativeSolver.");

    if (!(dataModel = dynamic_cast<BIO_DM_NS::IComposite2D*>(solver->getData())))
        throw Exception("InjectedElectrode: DataModel must implement IComposite2D.");


    this->solver = solver;
    this->structAnalyzer = new BIO_CFG_NS::StructureAnalyzer(solver->getConfig());
    this->mediumName = mediumName;
    this->substanceName = substanceName;
    this->substanceIndex = structAnalyzer->getSubstanceIndex(substanceName);
    this->reactionSpeed = structAnalyzer->getSymbol(reactionSpeedSymbolName)->value();

    this->areaIntegrator = new ConcentrationIntegralOverArea(
        solver,
        mediumName,
        substanceName,
        structAnalyzer
    );

    this->calculatedOutput = 0.0;
    this->calculatedOutputForStep = -1;
}




/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::AmperometricInjectedElectrode2D::~AmperometricInjectedElectrode2D()
{
    delete areaIntegrator;
    delete structAnalyzer;
    areas.clear();
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_TRD_NS::AmperometricInjectedElectrode2D::getOutput()
{
    BIO_SLV_NS::IIterativeSolver* iterative = dynamic_cast<BIO_SLV_NS::IIterativeSolver*>(solver);
    if (calculatedOutputForStep == iterative->getSolvedIterationCount())
    {
        return calculatedOutput;
    }

    LOG_DEBUG(LOGGER << "getOutput()...");

    double integralValue = areaIntegrator->integrate();

    integralValue *= CONST_F * CONST_n_e * reactionSpeed;

    //
    //  Divide by surface.
    //
    if (structAnalyzer->isCoordinateSystemCylindrical())
    {
        //
        //   integrate by angle (\fi) and divide by an area of a circle:
        //      ((2 \pi) / (\pi r^2))
        //
        double cellRadius = structAnalyzer->getPointsH()[structAnalyzer->getPointsH().size() - 1]->value();
        integralValue *= 2.0 / (cellRadius * cellRadius);
    }
    else
    {
        //
        //  We are integrated all in one line, now just divide all by it`s lenght.
        //
        double areaWidth = structAnalyzer->getPointsH()[structAnalyzer->getPointsH().size() - 1]->value();
        integralValue /= areaWidth;
    }



    calculatedOutput = integralValue;
    calculatedOutputForStep = iterative->getSolvedIterationCount();

    LOG_DEBUG(LOGGER << "getOutput()... Done, result=" << integralValue);

    return integralValue;
}


/* ************************************************************************** */
/* ************************************************************************** */
