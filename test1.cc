#include "test1.hh"
#include "dm/AbstractDM.hh"
#include "dm/ArrayDM.hh"
#include "sa/AbstractSA.hh"
#include "sa/BasicExplicitSA.hh"
#include "Config.hh"
#include <iostream>
#include <list>
#include <string>


class TestPoint : public dm::Point
{
    virtual double getSubstance(int substanceNr)
    {
        return 0;
    }
};
class TestPointFactory : public dm::PointFactory
{
    virtual dm::Point* newPoint()
    {
        return new TestPoint();
    }
};

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
        bound->getConditions().push_back(new cfg::Bound::WallCondition(substanceS));
        bound->getConditions().push_back(new cfg::Bound::WallCondition(substanceP));
        config->getBounds().push_back(bound = new cfg::Bound(1, 0, cfg::Bound::RIGHT));
        bound->getConditions().push_back(new cfg::Bound::WallCondition(substanceS));
        bound->getConditions().push_back(new cfg::Bound::WallCondition(substanceP));
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
        std::list<Dimension*> dimH;
        std::list<Dimension*> dimV;
        dimH.push_back(new ConstantDimension(Dimension::HORIZONTAL,  0.0, 1E-3, 100));
        dimH.push_back(new ConstantDimension(Dimension::HORIZONTAL, 1E-3, 1E-3, 100));
        dimV.push_back(new ConstantDimension(Dimension::VERTICAL,    0.0, 1E-3, 100));
        dimV.push_back(new ConstantDimension(Dimension::VERTICAL,   1E-3, 1E-3, 100));


        dm::PointFactory* pointFactory = new TestPointFactory();
        dm::ModelFactory* modelFactory = new dm::ArrayModelFactory();
        dm::Model* model = new dm::Model(
                               config,
                               pointFactory,
                               modelFactory,
                               dimH.size(), dimV.size(),
                               dimH, dimV
                           );

        // DO SOMETHING - Start.

        sa::Solver* solver = new sa::basicexplicit::Solver(model);
        solver->solve();
        delete solver;

        // DO SOMETHING - Done.

        delete model;
        delete modelFactory;
        delete pointFactory;



        for ( ; dimH.size() > 0; delete *dimH.begin(), dimH.pop_front());
        for ( ; dimV.size() > 0; delete *dimV.begin(), dimV.pop_front());

    }
    delete config;


    std::cout << "Running test1... Done\n";
    return 0;
}
