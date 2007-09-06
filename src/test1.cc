#include "test1.hh"
#include "dm_Abstract.hh"
#include "dm_Array.hh"
#include "sa_Abstract.hh"
#include "sa_BasicExplicit.hh"
#include "cfg_Config.hh"
#include "cfg_ConfigAnalyzer.hh"
#include "xsd/Model.hh"
#include "SolveListener.hh"
#include "Exceptions.hh"
#include <iostream>
#include <fstream>
#include <list>
#include <vector>
#include <string>
#include <log4cxx/logger.h>


int main()
{
    ::log4cxx::LoggerPtr logger(::log4cxx::Logger::getLogger("bio.test1"));
    LOG4CXX_INFO(logger, "Starting");
    try
    {
        std::auto_ptr<xsd::model::Model> model (xsd::model::model("src/xsd/Model-example-2D.xml"));

        cfg::ConfigAnalyzer2D analyzer(&*model);
        analyzer.analyze();

        LOG4CXX_INFO(logger, "Success");
    }
    catch (const xml_schema::exception& e)
    {
        LOG4CXX_ERROR(logger, e);
    }
    catch (Exception& ee)
    {
        LOG4CXX_ERROR(logger, ee.what());
    }
    LOG4CXX_INFO(logger, "Done");


    return 0;
    /* ********************************************************************** */
    /* ********************************************************************** */
    std::cout << "Running test1...\n";

    cfg::Config* config = 0;
    cfg::Substance* substanceS = 0;
    cfg::Substance* substanceP = 0;
    {
        substanceS = new cfg::Substance("S");
        substanceP = new cfg::Substance("P");

        cfg::Reaction* reaction = new cfg::MichaelisMentenReaction(
                                      "R",              // Name
                                      substanceS,       // Substrate
                                      substanceP,       // Product
                                      1E-8,             // V_max
                                      1E-7              // K_M
                                  );

        cfg::Medium* medEnzyme   = new cfg::Medium("Enzyme");
        cfg::Medium* medPerfMemb = new cfg::Medium("PerforatedMembrane");
        cfg::Medium* medSelMemb  = new cfg::Medium("SelectiveMembrane");
        medEnzyme->getDiffusions().push_back(new cfg::Medium::Diffusion(substanceS, 3E-6));
        medEnzyme->getDiffusions().push_back(new cfg::Medium::Diffusion(substanceP, 3E-6));
        medEnzyme->getReactions().push_back(reaction);
        medSelMemb->getDiffusions().push_back(new cfg::Medium::Diffusion(substanceP, 1.0E-8));

        config = new cfg::Config();
        config->getSubstances().push_back(substanceS);
        config->getSubstances().push_back(substanceP);
        config->getReactions().push_back(reaction);
        config->getMediums().push_back(medEnzyme);
        config->getMediums().push_back(medPerfMemb);
        config->getMediums().push_back(medSelMemb);
        config->getDimensionXParts().push_back(new cfg::ConstantDimensionPart(1.0E-5, 50));
        config->getDimensionXParts().push_back(new cfg::ConstantDimensionPart(1.0E-3, 50));
        config->getDimensionYParts().push_back(new cfg::ConstantDimensionPart(1.0E-3, 50));
        config->getDimensionYParts().push_back(new cfg::ConstantDimensionPart(2.0E-4, 50));
        config->getDimensionYParts().push_back(new cfg::ConstantDimensionPart(2.0E-4, 50));
        config->getAreas().push_back(new cfg::Area(0, 0, medEnzyme));
        config->getAreas().push_back(new cfg::Area(1, 0, medPerfMemb));
        config->getAreas().push_back(new cfg::Area(0, 1, medEnzyme));
        config->getAreas().push_back(new cfg::Area(1, 1, medEnzyme));
        config->getAreas().push_back(new cfg::Area(0, 2, medSelMemb));
        config->getAreas().push_back(new cfg::Area(1, 2, medSelMemb));

        cfg::Bound* bound = 0;

        config->getBounds().push_back(bound = new cfg::Bound(0, 0, cfg::Bound::TOP));
        bound->getConditions().push_back(new cfg::Bound::ConstantCondition(substanceS, 7E-5));
        bound->getConditions().push_back(new cfg::Bound::ConstantCondition(substanceP, 0.0));
        config->getBounds().push_back(bound = new cfg::Bound(0, 0, cfg::Bound::RIGHT));
        bound->getConditions().push_back(new cfg::Bound::WallCondition(substanceS));
        bound->getConditions().push_back(new cfg::Bound::WallCondition(substanceP));
        config->getBounds().push_back(bound = new cfg::Bound(0, 0, cfg::Bound::BOTTOM));
        bound->getConditions().push_back(new cfg::Bound::MergeCondition(substanceS));
        bound->getConditions().push_back(new cfg::Bound::MergeCondition(substanceP));
        config->getBounds().push_back(bound = new cfg::Bound(0, 0, cfg::Bound::LEFT));
        bound->getConditions().push_back(new cfg::Bound::WallCondition(substanceS));
        bound->getConditions().push_back(new cfg::Bound::WallCondition(substanceP));

        config->getBounds().push_back(bound = new cfg::Bound(1, 0, cfg::Bound::TOP));
        bound->getConditions().push_back(new cfg::Bound::ConstantCondition(substanceS, 0.0));
        bound->getConditions().push_back(new cfg::Bound::ConstantCondition(substanceP, 0.0));
        config->getBounds().push_back(bound = new cfg::Bound(1, 0, cfg::Bound::RIGHT));
        bound->getConditions().push_back(new cfg::Bound::ConstantCondition(substanceS, 0.0));
        bound->getConditions().push_back(new cfg::Bound::ConstantCondition(substanceP, 0.0));
        config->getBounds().push_back(bound = new cfg::Bound(1, 0, cfg::Bound::BOTTOM));
        bound->getConditions().push_back(new cfg::Bound::WallCondition(substanceS));
        bound->getConditions().push_back(new cfg::Bound::WallCondition(substanceP));
        config->getBounds().push_back(bound = new cfg::Bound(1, 0, cfg::Bound::LEFT));
        bound->getConditions().push_back(new cfg::Bound::WallCondition(substanceS));
        bound->getConditions().push_back(new cfg::Bound::WallCondition(substanceP));

        config->getBounds().push_back(bound = new cfg::Bound(0, 1, cfg::Bound::TOP));
        bound->getConditions().push_back(new cfg::Bound::MergeCondition(substanceS));
        bound->getConditions().push_back(new cfg::Bound::MergeCondition(substanceP));
        config->getBounds().push_back(bound = new cfg::Bound(0, 1, cfg::Bound::RIGHT));
        bound->getConditions().push_back(new cfg::Bound::MergeCondition(substanceS));
        bound->getConditions().push_back(new cfg::Bound::MergeCondition(substanceP));
        config->getBounds().push_back(bound = new cfg::Bound(0, 1, cfg::Bound::BOTTOM));
        bound->getConditions().push_back(new cfg::Bound::WallCondition(substanceS));
        bound->getConditions().push_back(new cfg::Bound::MergeCondition(substanceP));
        config->getBounds().push_back(bound = new cfg::Bound(0, 1, cfg::Bound::LEFT));
        bound->getConditions().push_back(new cfg::Bound::WallCondition(substanceS));
        bound->getConditions().push_back(new cfg::Bound::WallCondition(substanceP));

        config->getBounds().push_back(bound = new cfg::Bound(1, 1, cfg::Bound::TOP));
        bound->getConditions().push_back(new cfg::Bound::WallCondition(substanceS));
        bound->getConditions().push_back(new cfg::Bound::WallCondition(substanceP));
        config->getBounds().push_back(bound = new cfg::Bound(1, 1, cfg::Bound::RIGHT));
        bound->getConditions().push_back(new cfg::Bound::WallCondition(substanceS));
        bound->getConditions().push_back(new cfg::Bound::WallCondition(substanceP));
        config->getBounds().push_back(bound = new cfg::Bound(1, 1, cfg::Bound::BOTTOM));
        bound->getConditions().push_back(new cfg::Bound::WallCondition(substanceS));
        bound->getConditions().push_back(new cfg::Bound::MergeCondition(substanceP));
        config->getBounds().push_back(bound = new cfg::Bound(1, 1, cfg::Bound::LEFT));
        bound->getConditions().push_back(new cfg::Bound::MergeCondition(substanceS));
        bound->getConditions().push_back(new cfg::Bound::MergeCondition(substanceP));

        config->getBounds().push_back(bound = new cfg::Bound(0, 2, cfg::Bound::TOP));
        bound->getConditions().push_back(new cfg::Bound::WallCondition(substanceS));
        bound->getConditions().push_back(new cfg::Bound::MergeCondition(substanceP));
        config->getBounds().push_back(bound = new cfg::Bound(0, 2, cfg::Bound::RIGHT));
        bound->getConditions().push_back(new cfg::Bound::ConstantCondition(substanceS, 0.0));
        bound->getConditions().push_back(new cfg::Bound::MergeCondition(substanceP));
        config->getBounds().push_back(bound = new cfg::Bound(0, 2, cfg::Bound::BOTTOM));
        bound->getConditions().push_back(new cfg::Bound::ConstantCondition(substanceS, 0.0));
        bound->getConditions().push_back(new cfg::Bound::ElectrodeCondition(substanceP));
        config->getBounds().push_back(bound = new cfg::Bound(0, 2, cfg::Bound::LEFT));
        bound->getConditions().push_back(new cfg::Bound::ConstantCondition(substanceS, 0.0));
        bound->getConditions().push_back(new cfg::Bound::WallCondition(substanceP));

        config->getBounds().push_back(bound = new cfg::Bound(1, 2, cfg::Bound::TOP));
        bound->getConditions().push_back(new cfg::Bound::WallCondition(substanceS));
        bound->getConditions().push_back(new cfg::Bound::MergeCondition(substanceP));
        config->getBounds().push_back(bound = new cfg::Bound(1, 2, cfg::Bound::RIGHT));
        bound->getConditions().push_back(new cfg::Bound::ConstantCondition(substanceS, 0.0));
        bound->getConditions().push_back(new cfg::Bound::WallCondition(substanceP));
        config->getBounds().push_back(bound = new cfg::Bound(1, 2, cfg::Bound::BOTTOM));
        bound->getConditions().push_back(new cfg::Bound::ConstantCondition(substanceS, 0.0));
        bound->getConditions().push_back(new cfg::Bound::ElectrodeCondition(substanceP));
        config->getBounds().push_back(bound = new cfg::Bound(1, 2, cfg::Bound::LEFT));
        bound->getConditions().push_back(new cfg::Bound::ConstantCondition(substanceS, 0.0));
        bound->getConditions().push_back(new cfg::Bound::MergeCondition(substanceP));
    }
    {
        dm::ModelFactory* modelFactory = new dm::ArrayModelFactory();
        sa::Solver* solver = new sa::basicexplicit::Solver(config, modelFactory);
        sa::SolveListener* listeners[100];
        int                lc = 0;

        std::ofstream fS;
        std::ofstream fP;
        fS.open("test1-S.dat");
        fP.open("test1-P.dat");

        int finish = 1000;
        solver->addListener(listeners[lc++] = new sl::ErrorInDataListener(100));
        solver->addListener(listeners[lc++] = new sl::GnuplotDataSL(fS, finish, substanceS));
        solver->addListener(listeners[lc++] = new sl::GnuplotDataSL(fP, finish, substanceP));
        solver->addListener(listeners[lc++] = new sl::StopAtStepSL(finish));
        solver->setTimeStep(5E-9);

        solver->solve();

        for (int i = 0; i < lc; delete listeners[i++])
            ;

        fS.close();
        fP.close();

        delete solver;
        delete modelFactory;
    }
    delete config;

    std::cout << "Running test1... Done\n";
    return 0;
}
