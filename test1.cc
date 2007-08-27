#include "test1.hh"
#include "dm/AbstractDM.hh"
#include "dm/ArrayDM.hh"
#include "sa/AbstractSA.hh"
#include "sa/BasicExplicitSA.hh"
#include "Config.hh"
#include "SolveListener.hh"
#include <iostream>
#include <list>
#include <string>


int main()
{
    std::cout << "Running test1...\n";

    cfg::Config* config = 0;
    {
        cfg::Substance* substanceS = new cfg::Substance("S");
        cfg::Substance* substanceP = new cfg::Substance("P");
        cfg::Reaction* reaction = new cfg::MichaelisMentenReaction("R", substanceS, substanceP, 1E-8, 1E-8);

        cfg::Medium* medEnzyme = new cfg::Medium("Enzyme");
        cfg::Medium* medMembrane = new cfg::Medium("PerforatedMembrane");
        medEnzyme->getDiffusions().push_back(new cfg::Medium::Diffusion(substanceS, 1E-6));
        medEnzyme->getDiffusions().push_back(new cfg::Medium::Diffusion(substanceP, 1E-6));
        medEnzyme->getReactions().push_back(reaction);

        config = new cfg::Config();
        config->getSubstances().push_back(substanceS);
        config->getSubstances().push_back(substanceP);
        config->getReactions().push_back(reaction);
        config->getMediums().push_back(medEnzyme);
        config->getMediums().push_back(medMembrane);
        config->getDimensionXParts().push_back(new cfg::ConstantDimensionPart(1E-3, 100));
        config->getDimensionXParts().push_back(new cfg::ConstantDimensionPart(1E-3, 100));
        config->getDimensionYParts().push_back(new cfg::ConstantDimensionPart(1E-3, 100));
        config->getDimensionYParts().push_back(new cfg::ConstantDimensionPart(1E-3, 100));
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
        sl::DebugSL *slDebug1 = new sl::DebugSL(std::cout, 0 );
        sl::DebugSL *slDebug2 = new sl::DebugSL(std::cout, 1 );
        sl::DebugSL *slDebug3 = new sl::DebugSL(std::cout, 10);
        sl::DebugSL *slDebug4 = new sl::DebugSL(std::cout, 20);
        sl::DebugSL *slDebug5 = new sl::DebugSL(std::cout, 99);
        sl::DebugSL *slDebug6 = new sl::DebugSL(std::cout, 999);
        solver->addListener(slDebug1);
        solver->addListener(slDebug2);
        solver->addListener(slDebug3);
        solver->addListener(slDebug4);
        solver->addListener(slDebug5);
        solver->addListener(slDebug6);

        solver->solve();
        delete slDebug1;
        delete slDebug2;
        delete slDebug3;
        delete slDebug4;
        delete slDebug5;
        delete slDebug6;
        delete solver;
        delete modelFactory;
    }
    delete config;


    std::cout << "Running test1... Done\n";
    return 0;
}
