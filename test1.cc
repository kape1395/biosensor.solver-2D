#include "test1.hh"
#include "dm/Abstract.hh"
#include "dm/Array.hh"
#include "sa/BasicExplicit.hh"
#include <iostream>
#include <list>

class TestPoint : public dm::Point { virtual double getSubstance(int substanceNr) { return 0; } };
class TestPointFactory : public dm::PointFactory { virtual dm::Point* newPoint() { return new TestPoint(); } };

int main()
{
    std::cout << "Running test1...\n";


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



    std::cout << "Running test1... Done\n";
    return 0;
}
