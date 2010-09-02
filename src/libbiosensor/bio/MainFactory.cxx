#include "MainFactory.hxx"
#include "IFactory.hxx"
#include "io/ConcentrationProfile.hxx"
#include "io/CurrentDensity.hxx"
#include "io/AveragedConcentration.hxx"
#include "slv/AdjustTimeStepByFactor.hxx"
#include "slv/AdjustTimeStepAdaptively.hxx"
#include "slv/StopAtSpecifiedPoint.hxx"
#include "slv/StopByCurrentDensityGradient.hxx"
#include "slv/StopIfConcentrationsOscillateBySpace.hxx"
#include "slv/StopIfInvalidConcentrations.hxx"
#include "slv/StopIfSumOfConcentrationsNonConst.hxx"
#include "slv/InvokeNotBefore.hxx"
#include "slv/InvokeEveryTimeStep.hxx"
#include "trd/AmperometricElectrode2DOnBound.hxx"
#include "trd/AmperometricInjectedElectrode2D.hxx"
#include "trd/CompositeElectrode.hxx"
#include "Exception.hxx"
#include "Logging.hxx"
#define LOGGER "libbiosensor::MainFactory: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_NS::MainFactory::MainFactory(
    BIO_NS::IFactory* rootFactory,
    BIO_IO_NS::IContext* context
)
{
    this->rootFactory = rootFactory;
    this->context = context;
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
    else if (dynamic_cast<BIO_XML_MODEL_NS::solver::FailOnConcentrationOscillation*>(stopCondition))
    {
        /* ****************************************************************** */
        /* ****************************************************************** */

        return new BIO_SLV_NS::StopIfConcentrationsOscillateBySpace(solver);

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
BIO_SLV_NS::ISolverListener* BIO_NS::MainFactory::createTimeStepAdjuster(
    BIO_SLV_NS::ISolver* solver,
    BIO_XML_MODEL_NS::solver::TimeStepAdjuster* timeStepAdjuster
)
{
    if (dynamic_cast<BIO_XML_MODEL_NS::solver::AdaptiveTimeStepAdjuster*>(timeStepAdjuster))
    {
        using BIO_XML_MODEL_NS::solver::AdaptiveTimeStepAdjuster;
        AdaptiveTimeStepAdjuster* adaptiveTSA = dynamic_cast<AdaptiveTimeStepAdjuster*>(timeStepAdjuster);

        std::vector<BIO_SLV_NS::ISolverListener*> stopConditions;
        for (AdaptiveTimeStepAdjuster::stopCondition_iterator it = adaptiveTSA->stopCondition().begin();
                it < adaptiveTSA->stopCondition().end(); it++)
        {
            stopConditions.push_back(rootFactory->createStopCondition(solver, &*it));
        }

        BIO_SLV_NS::ISolverListener* cwListener = rootFactory->createOutput(solver, &adaptiveTSA->stateStore());
        BIO_IO_NS::ConcentrationProfile* cw = dynamic_cast<BIO_IO_NS::ConcentrationProfile*>(cwListener);
        if (!cw)
            throw new BIO_NS::Exception("Concentration profile writer must be specified for the AdaptiveTimeStepAdjuster");

        BIO_SLV_NS::AdjustTimeStepAdaptively* tsa = new BIO_SLV_NS::AdjustTimeStepAdaptively(
            solver,
            adaptiveTSA->increase().factor(),
            adaptiveTSA->increase().everyStepCount(),
            adaptiveTSA->increase().maxStepSize(),
            adaptiveTSA->fallback().factor(),
            adaptiveTSA->fallback().checkEveryStepCount(),
            adaptiveTSA->fallback().minStepSize(),
            cw,
            stopConditions
        );

        return tsa;
    }
    else if (dynamic_cast<BIO_XML_MODEL_NS::solver::SimpleTimeStepAdjuster*>(timeStepAdjuster))
    {
        using BIO_XML_MODEL_NS::solver::SimpleTimeStepAdjuster;
        SimpleTimeStepAdjuster* simpleTSA = dynamic_cast<SimpleTimeStepAdjuster*>(timeStepAdjuster);

        BIO_SLV_NS::AdjustTimeStepByFactor* tsa = new BIO_SLV_NS::AdjustTimeStepByFactor(
            solver,
            simpleTSA->factor(),
            simpleTSA->everyStepCount(),
            simpleTSA->maxStepSize().present() ? simpleTSA->maxStepSize().get() : 0.0
        );
        return tsa;
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
            context
        );

        return out;
        /* ****************************************************************** */
        /* ****************************************************************** */
    }
    else if (dynamic_cast<BIO_XML_MODEL_NS::solver::AveragedConcentration*>(output))
    {
        /* ****************************************************************** */
        /* ****************************************************************** */
        BIO_XML_MODEL_NS::solver::AveragedConcentration* outputConf;
        outputConf = dynamic_cast<BIO_XML_MODEL_NS::solver::AveragedConcentration*>(output);

        BIO_IO_NS::AveragedConcentration* out = new BIO_IO_NS::AveragedConcentration(
            output->name(),
            solver,
            context,
            outputConf->medium().present() ? &outputConf->medium().get() : 0
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
            context
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
                   transIE->reaction()
               );
    }
    else if (dynamic_cast<CompositeElectrode*>(transducer))
    {
        CompositeElectrode* transCE = dynamic_cast<CompositeElectrode*>(transducer);
        BIO_TRD_NS::CompositeElectrode* electrode = new BIO_TRD_NS::CompositeElectrode();

        for (CompositeElectrode::transducer_iterator it = transCE->transducer().begin(); it < transCE->transducer().end(); it++)
        {
            BIO_SLV_NS::ITransducer* subTrd = rootFactory->createTransducer(solver, &*it);
            if (subTrd)
            {
                electrode->addTransducer(subTrd, true);
            }
            else
            {
                LOG_ERROR(LOGGER << "I dond know how to create sub-transducer for CompositeElectrode.");
                delete electrode;
                return 0;
            }
        }

        return electrode;
    }
    return 0;
}

/* ************************************************************************** */
