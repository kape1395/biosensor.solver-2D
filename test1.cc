#include "test1.hh"
#include "dm/Abstract.hh"
#include "dm/Array.hh"
#include "sa/BasicExplicit.hh"
#include <iostream>


int main()
{
    std::cout << "Running test1...\n";


    {
        Dimension *dimH[2];
        Dimension *dimV[2];
        dimH[0] = new ConstantDimension(Dimension::HORIZONTAL,  0.0, 1E-3, 100);
        dimH[1] = new ConstantDimension(Dimension::HORIZONTAL, 1E-3, 1E-3, 100);
        dimV[0] = new ConstantDimension(Dimension::VERTICAL,    0.0, 1E-3, 100);
        dimV[1] = new ConstantDimension(Dimension::VERTICAL,   1E-3, 1E-3, 100);


        dm::Factory<sa::basicexplicit::PointData> *factory =
                new dm::ArrayFactory<sa::basicexplicit::PointData>();

        dm::Model<sa::basicexplicit::PointData, 2, 2>* model =
                new dm::Model<sa::basicexplicit::PointData, 2, 2>(factory, dimH, dimV);

        // DO SOMETHING - Start.
        
        sa:Solver solver = new sa::basixecplicit::Solver(model);
        solver->solve();
        
        // DO SOMETHING - Done.

        delete model;
        delete factory;


        for ( int i = 0; i < 2; i++ )
            delete dimH[i];

        for ( int j = 0; j < 2; j++ )
            delete dimV[j];
    }



    std::cout << "Running test1... Done\n";
    return 0;
}
