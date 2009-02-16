
#include "ModelSolver.hxx"

#include "MainFactory.hxx"
#include "io/ConcentrationProfile.hxx"
#include "io/CurrentDensity.hxx"
#include "slv/StopAtSpecifiedPoint.hxx"
#include "slv/StopByCurrentDensityGradient.hxx"
#include "slv/StopIfInvalidConcentrations.hxx"
#include "slv/StopIfSumOfConcentrationsNonConst.hxx"
#include "slv/InvokeNotBefore.hxx"
#include "slv/InvokeEveryTimeStep.hxx"
#include "trd/AmperometricElectrode2DOnBound.hxx"
#include "trd/AmperometricInjectedElectrode2D.hxx"
#include "Exception.hxx"


/* ************************************************************************** */
/* ************************************************************************** */
BIO_NS::MainFactory::MainFactory(
    BIO_NS::IFactory* rootFactory,
    BIO_IO_NS::IContext* Context
)
{
    this->rootFactory = rootFactory;
    this->Context = Context;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_NS::MainFactory::~MainFactory()
{
    //  Nothing to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::ISolver* BIO_NS::MainFactory::createSolver(
    BIO_XML_MODEL_NS::Model* model
)
{
    //  There is no solvers in the main library.
    return 0;
}


/* ************************************************************************** */
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
    else if (dynamic_cast<BIO_XML_MODEL_NS::solver::FailIfInvalidConcentrations*>(stopCondition))
    {
        /* ****************************************************************** */
        /* ****************************************************************** */

        return new BIO_SLV_NS::StopIfInvalidConcentrations(solver);

        /* ****************************************************************** */
        /* ****************************************************************** */
    }
    else if (dynamic_cast<BIO_XML_MODEL_NS::solver::FailISumOfConcentrationsNonConst*>(stopCondition))
    {
        /* ****************************************************************** */
        /* ****************************************************************** */

        BIO_XML_MODEL_NS::solver::FailISumOfConcentrationsNonConst* fail;
        fail = dynamic_cast<BIO_XML_MODEL_NS::solver::FailISumOfConcentrationsNonConst*>(stopCondition);

        std::vector<BIO_XML_MODEL_NS::SubstanceName*> substances;
        for (unsigned i = 0; i < fail->substance().size(); i++)
        {
            substances.push_back(&fail->substance()[i]);
        }

        BIO_SLV_NS::StopIfSumOfConcentrationsNonConst* stop;
        stop = new BIO_SLV_NS::StopIfSumOfConcentrationsNonConst(
            solver,
            fail->medium(),
            fail->sum(),
            fail->error(),
            substances
        );
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
        using BIO_XML_MODEL_NS::solver::CurrentDensityGradient;
        CurrentDensityGradient* scg = dynamic_cast<CurrentDensityGradient*>(stopCondition);

        BIO_SLV_NS::StopByCurrentDensityGradient* stop = new BIO_SLV_NS::StopByCurrentDensityGradient(
            solver,
            scg->lessThan(),
            scg->normalized()
        );
        return stop;
    }
    else
    {
        return 0;
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::ISolverListener* BIO_NS::MainFactory::createOutput(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::SolverOutput* output
)
{
    using namespace BIO_XML_MODEL_NS;
    using namespace BIO_XML_MODEL_NS::solver;

    if (dynamic_cast<BIO_XML_MODEL_NS::solver::Kinetic*>(output))
    {
        /* ****************************************************************** */
        /* ****************************************************************** */
        Kinetic* kinetic = dynamic_cast<Kinetic*>(output);

        if (!(kinetic->time().present() || kinetic->stepCount().present()))
            throw BIO_NS::Exception("At least one of time and stepCount must be specified for solver::Kinetic");


        BIO_SLV_NS::InvokeEveryTimeStep* out = new BIO_SLV_NS::InvokeEveryTimeStep(solver);

        if (kinetic->stepCount().present())
            out->setStepByStepCount(kinetic->stepCount().get());

        if (kinetic->time().present())
            out->setStepByTime(kinetic->time().get());


        Kinetic::output_sequence& subOuts = kinetic->output();
        for (Kinetic::output_iterator o = subOuts.begin(); o < subOuts.end(); o++)
        {
            BIO_SLV_NS::ISolverListener* subOut = rootFactory->createOutput(solver, &*o);
            if (dynamic_cast<BIO_IO_NS::IRepeatable*>(subOut))
            {
                dynamic_cast<BIO_IO_NS::IRepeatable*>(subOut)->setRepeatable(true);
            }
            out->addListener(subOut, true);
        }

        return out;
        /* ****************************************************************** */
        /* ****************************************************************** */
    }
    else if (dynamic_cast<BIO_XML_MODEL_NS::solver::SteadyState*>(output))
    {
        /* ****************************************************************** */
        /* ****************************************************************** */
        // TODO: Implement
        throw BIO_NS::Exception("BIO_XML_MODEL_NS::solver::SteadyState is not implemented yet");
        /* ****************************************************************** */
        /* ****************************************************************** */
    }
    else if (dynamic_cast<BIO_XML_MODEL_NS::solver::ConcentrationProfile*>(output))
    {
        /* ****************************************************************** */
        /* ****************************************************************** */
        BIO_IO_NS::ConcentrationProfile* out = new BIO_IO_NS::ConcentrationProfile(
            output->name(),
            solver,
            Context
        );

        return out;
        /* ****************************************************************** */
        /* ****************************************************************** */
    }
    else if (dynamic_cast<BIO_XML_MODEL_NS::solver::CurrentDensity*>(output))
    {
        /* ****************************************************************** */
        /* ****************************************************************** */
        BIO_IO_NS::CurrentDensity* out = new BIO_IO_NS::CurrentDensity(
            output->name(),
            solver,
            Context
        );

        return out;
        /* ****************************************************************** */
        /* ****************************************************************** */
    }
    else if (dynamic_cast<BIO_XML_MODEL_NS::solver::SteadyStateHalfTime*>(output))
    {
        /* ****************************************************************** */
        /* ****************************************************************** */
        // TODO: Implement
        throw BIO_NS::Exception("BIO_XML_MODEL_NS::solver::SteadyStateHalfTime is not implemented yet");
        /* ****************************************************************** */
        /* ****************************************************************** */
    }
    else
    {
        return 0;
    }
}


/* ************************************************************************** */
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
                   transIE->substance(),
                   transIE->reactionSpeed()
               );
    }
    return 0;
}

/* ************************************************************************** */
