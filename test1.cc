#include "test1.hh"
#include "dm/AbstractDM.hh"
#include "dm/ArrayDM.hh"
#include "sa/AbstractSA.hh"
#include "sa/BasicExplicitSA.hh"
#include "Config.hh"
#include "SolveListener.hh"
#include <iostream>
#include <fstream>
#include <list>
#include <string>


int main()
{
    std::cout << "Running test1...\n";

    cfg::Config* config = 0;
    cfg::Substance* substanceS = 0;
    cfg::Substance* substanceP = 0;
    {
        substanceS = new cfg::Substance("S");
        substanceP = new cfg::Substance("P");

        cfg::Reaction* reaction = new cfg::MichaelisMentenReaction("R", substanceS, substanceP, 1E-8, 1E-8);

        cfg::Medium* medEnzyme = new cfg::Medium("Enzyme");
        cfg::Medium* medMembrane = new cfg::Medium("PerforatedMembrane");
        medEnzyme->getDiffusions().push_back(new cfg::Medium::Diffusion(substanceS, 1E-5)); //  1E-6
        medEnzyme->getDiffusions().push_back(new cfg::Medium::Diffusion(substanceP, 1E-5)); //  1E-6
        medEnzyme->getReactions().push_back(reaction);

        config = new cfg::Config();
        config->getSubstances().push_back(substanceS);
        config->getSubstances().push_back(substanceP);
        config->getReactions().push_back(reaction);
        config->getMediums().push_back(medEnzyme);
        config->getMediums().push_back(medMembrane);
        config->getDimensionXParts().push_back(new cfg::ConstantDimensionPart(1E-3, 150));
        config->getDimensionXParts().push_back(new cfg::ConstantDimensionPart(1E-3, 150));
        config->getDimensionYParts().push_back(new cfg::ConstantDimensionPart(1E-3, 150));
        config->getDimensionYParts().push_back(new cfg::ConstantDimensionPart(1E-3, 150));
        config->getAreas().push_back(new cfg::Area(0, 0, medEnzyme));
        config->getAreas().push_back(new cfg::Area(1, 0, medMembrane));
        config->getAreas().push_back(new cfg::Area(0, 1, medEnzyme));
        config->getAreas().push_back(new cfg::Area(1, 1, medEnzyme));

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
        bound->getConditions().push_back(new cfg::Bound::ElectrodeCondition(substanceP));
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
        bound->getConditions().push_back(new cfg::Bound::ElectrodeCondition(substanceP));
        config->getBounds().push_back(bound = new cfg::Bound(1, 1, cfg::Bound::LEFT));
        bound->getConditions().push_back(new cfg::Bound::MergeCondition(substanceS));
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

        int finish = 1000000;
      //solver->addListener(listeners[lc++] = new sl::DebugSL(std::cout, 100));
        solver->addListener(listeners[lc++] = new sl::ErrorInDataListener(100));
        solver->addListener(listeners[lc++] = new sl::GnuplotDataSL(fS, finish, substanceS));
        solver->addListener(listeners[lc++] = new sl::GnuplotDataSL(fP, finish, substanceP));
        solver->addListener(listeners[lc++] = new sl::StopAtStepSL(finish));
        solver->setTimeStep(1E-6);

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
