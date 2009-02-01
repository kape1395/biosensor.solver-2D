#include "MainFactory.hxx"
#include "slv/StopAtSpecifiedPoint.hxx"
#include "slv/InvokeNotBefore.hxx"
#include "trd/AmperometricElectrode2DOnBound.hxx"
#include "trd/AmperometricInjectedElectrode2D.hxx"
#include "Exception.hxx"


/* ************************************************************************** */
BIO_NS::MainFactory::MainFactory(IFactory* rootFactory)
{
    this->rootFactory = rootFactory;
}


/* ************************************************************************** */
BIO_NS::MainFactory::~MainFactory()
{
    //  Nothing to do here.
}


/* ************************************************************************** */
BIO_SLV_NS::ISolver* BIO_NS::MainFactory::createSolver(
    BIO_XML_MODEL_NS::Model* model
)
{
    //  There is no solvers in the main library.
    return 0;
}


/* ************************************************************************** */
BIO_SLV_NS::ISolverListener* BIO_NS::MainFactory::createStopCondition(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::solver::StopCondition* stopCondition
)
{
    if (dynamic_cast<BIO_XML_MODEL_NS::solver::FailIfAbove*>(stopCondition))
    {
        /* ****************************************************************** */
        /* ****************************************************************** */
        BIO_XML_MODEL_NS::solver::FailIfAbove* fia;
        fia = dynamic_cast<BIO_XML_MODEL_NS::solver::FailIfAbove*>(stopCondition);
        BIO_SLV_NS::StopAtSpecifiedPoint* stop = new BIO_SLV_NS::StopAtSpecifiedPoint(solver);
        if (fia->stepCount().present())
        {
            stop->setStepCount(fia->stepCount().get());
        }
        if (fia->time().present())
        {
            stop->setTime(fia->time().get());
        }
        return stop;
        /* ****************************************************************** */
        /* ****************************************************************** */
    }
    else if (dynamic_cast<BIO_XML_MODEL_NS::solver::StopConditionValidAfter*>(stopCondition))
    {
        /* ****************************************************************** */
        /* ****************************************************************** */
        using BIO_XML_MODEL_NS::solver::StopConditionValidAfter;
        StopConditionValidAfter* scva = dynamic_cast<StopConditionValidAfter*>(stopCondition);
        BIO_SLV_NS::InvokeNotBefore* stop = new BIO_SLV_NS::InvokeNotBefore(solver);

        //  Set stepCount.
        if (scva->stepCount().present())
            stop->setStepCount(scva->stepCount().get());

        //  Set time
        if (scva->time().present())
            stop->setTime(scva->time().get());

        //  Add all sub stop conditions.
        StopConditionValidAfter::stopCondition_sequence& subStops = scva->stopCondition();
        for (StopConditionValidAfter::stopCondition_iterator sc = subStops.begin(); sc < subStops.end(); sc++)
        {
            stop->addListener(rootFactory->createStopCondition(solver, &*sc), true);
        }
        return stop;
        /* ****************************************************************** */
        /* ****************************************************************** */
    }
    else if (dynamic_cast<BIO_XML_MODEL_NS::solver::CurrentDensityGradient*>(stopCondition))
    {
        throw Exception("StopCondition CurrentDensityGradient not implemented yet");
    }
    return 0;
}


/* ************************************************************************** */
BIO_SLV_NS::ISolverListener* BIO_NS::MainFactory::createOutput(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::SolverOutput* output
)
{
    //  TODO: Implement
    return 0;
}


/* ************************************************************************** */
BIO_SLV_NS::ITransducer* BIO_NS::MainFactory::createTransducer(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::Transducer* transducer
)
{
    using namespace BIO_XML_MODEL_NS::transducer;
    if (dynamic_cast<AmperometricElectrode*>(transducer))
    {
        AmperometricElectrode* transAE = dynamic_cast<AmperometricElectrode*>(transducer);
        return new BIO_TRD_NS::AmperometricElectrode2DOnBound(
                   solver,
                   transAE->bound(),
                   transAE->substance()
               );
    }
    else if (dynamic_cast<InjectedElectrode*>(transducer))
    {
        InjectedElectrode* transIE = dynamic_cast<InjectedElectrode*>(transducer);
        return new BIO_TRD_NS::AmperometricInjectedElectrode2D(
                   solver,
                   transIE->medium(),
                   transIE->substance()
               );
    }
    return 0;
}

/* ************************************************************************** */
