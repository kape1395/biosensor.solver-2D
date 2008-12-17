
#include "Model.hxx"


#include "ModelMediumReaction.hxx"

#include "AreaSubSolver.hxx"
#include <bio/Exception.hxx>

/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::AreaSubSolver::AreaSubSolver(
    Solver *solver,
    int positionH,
    int positionV,
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
    BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer* fdAnalyzer
) : log(log4cxx::Logger::getLogger("libbiosensor-slv-fd::im2d::AreaSubSolver"))
{
    using BIO_XML_NS::model::s::Axis;
    using BIO_XML_NS::model::s::ConstantAxisPart;

    LOG4CXX_DEBUG(log, "AreaSubSolver()");
    this->solver = solver;
    this->positionH = positionH;
    this->positionV = positionV;

    
    ////////////////////////////////////////////////////////////////////////////
    //  Create data structures
    //
    ConstantAxisPart* axisPartH = dynamic_cast<ConstantAxisPart*> (fdAnalyzer->getAxisPartH(positionH));
    ConstantAxisPart* axisPartV = dynamic_cast<ConstantAxisPart*> (fdAnalyzer->getAxisPartV(positionV));
    if (axisPartH == 0 || axisPartV == 0)
    {
        LOG4CXX_ERROR(log, "Only axis parts of type ConstantAxisPart are now supported.");
        throw Exception("Invalid coonf.");
    }

    this->dataSizeH = axisPartH->stepCount() + 1;
    this->dataSizeV = axisPartV->stepCount() + 1;
    this->dataSizeS = structAnalyzer->getSubstances().size();

    this->stepSizeH = (structAnalyzer->getSymbol(axisPartH->to())->value() -
                       structAnalyzer->getSymbol(axisPartH->from())->value()) /
                      axisPartH->stepCount();

    this->stepSizeV = (structAnalyzer->getSymbol(axisPartV->to())->value() -
                       structAnalyzer->getSymbol(axisPartV->from())->value()) /
                      axisPartV->stepCount();

    this->startPositionH = structAnalyzer->getSymbol(axisPartH->from())->value();
    this->startPositionV = structAnalyzer->getSymbol(axisPartV->from())->value();

    this->layersInverted = false;

    data = new double***[dataSizeH];
    for (int h = 0; h < dataSizeH; h++)
    {
        data[h] = new double**[dataSizeV];
        for (int v = 0; v < dataSizeV; v++)
        {
            data[h][v] = new double*[dataSizeS];
            for (int s = 0; s < dataSizeS; s++)
            {
                data[h][v][s] = new double[5];

                // Apply initial conditions.
                data[h][v][s][0] = structAnalyzer->getInitialConcentration(
                                       s, positionH, positionH
                                   )->value();
            }
        }
    }
    //
    //  Create data structures
    ////////////////////////////////////////////////////////////////////////////


    ////////////////////////////////////////////////////////////////////////////
    //  Check the coordinate system
    if (structAnalyzer->isCoordinateSystemCartesian())
    {
        coordinateSystemIsCartesian = true;
        coordinateSystemIsCylindrical = false;
    }
    else if (structAnalyzer->isCoordinateSystemCylindrical())
    {
        coordinateSystemIsCartesian = false;
        coordinateSystemIsCylindrical = true;
    }
    else
    {
        throw Exception("Unsupported coordinate system");
    }
    //  Check the coordinate system
    ////////////////////////////////////////////////////////////////////////////


    ////////////////////////////////////////////////////////////////////////////
    // Collect information about diffusions
    {
        //
        //  NOTE:   The cineticts is modelled only for substances, for which
        //          the diffusion is defined.
        //
        BIO_XML_NS::model::Medium *medium = structAnalyzer->getMedium(posutionH, positionV);
        this->substIndexes = new int[this->substCount = medium->diffusion().size()];
        
        this->D = new double[dataSizeS];
        int localIndex = 0;
        for (int i = 0; i < dataSizeS; i++)
        {
            BIO_XML_NS::model::Symbol *sym = structAnalyzer->getDiffusion(i, positionH, positionV);
            D[i] = sym == 0 ? 0.0 : sym->value();
            if (sym)
            {
                substIndexes[localIndex++] = i;
            }
        }
    }
    // Collect information about diffusions
    ////////////////////////////////////////////////////////////////////////////


    ////////////////////////////////////////////////////////////////////////////
    // Collect information about reactions
    {
        typedef std::vector< BIO_XML_NS::model::MediumReaction* > reactions_vector;
        typedef BIO_XML_NS::model::mr::MichaelisMenten R_MM;
        typedef BIO_XML_NS::model::mr::ReductionOxidation R_RO;

        reactions_vector& reactions = structAnalyzer->getReactions(positionH, positionV);
        std::vector< ReactionMMPart > *mmParts = new std::vector< ReactionMMPart >[dataSizeS];
        std::vector< ReactionROPart > *roParts = new std::vector< ReactionROPart >[dataSizeS];
        for (
            reactions_vector::iterator reaction = reactions.begin();
            reaction < reactions.end();
            reaction++
        )
        {
            R_MM *rMM = dynamic_cast< R_MM* >(*reaction);
            R_RO *rRO = dynamic_cast< R_RO* >(*reaction);
            if (rMM != 0)
            {
                int substS = structAnalyzer->getSubstanceIndex(rMM->substrate());
                int substP = structAnalyzer->getSubstanceIndex(rMM->product());
                ReactionMMPart partS;
                ReactionMMPart partP;
                partS.substrateIndex = partP.substrateIndex = substS;
                partS.V_max = + structAnalyzer->getSymbol(rMM->V_max())->value();
                partP.V_max = - structAnalyzer->getSymbol(rMM->V_max())->value();
                partS.K_M = partP.K_M = structAnalyzer->getSymbol(rMM->K_M())->value();
                mmParts[substS].push_back(partS);
                mmParts[substP].push_back(partP);
            }
            else if (rRO != 0)
            {
                if (rRO->substrates().size() != 2 || rRO->products().size() != 2 ||
                        rRO->substrates()[0].coefficient() != 1 ||
                        rRO->substrates()[1].coefficient() != 1 ||
                        rRO->products()[0].coefficient() != 1 ||
                        rRO->products()[1].coefficient() != 1
                   )
                {
                    LOG4CXX_ERROR(log, "Implementation of RO reaction is limited: 2S + 2P without coefficients");
                    throw Exception("Unsupported reaction.");
                }

                int substS1 = structAnalyzer->getSubstanceIndex(rRO->substrates()[0].name());
                int substS2 = structAnalyzer->getSubstanceIndex(rRO->substrates()[1].name());
                int substP1 = structAnalyzer->getSubstanceIndex(rRO->products()[0].name());
                int substP2 = structAnalyzer->getSubstanceIndex(rRO->products()[1].name());
                double rate = structAnalyzer->getSymbol(rRO->rate())->value();

                ReactionROPart part;
                part.substrate1Index = substS1;
                part.substrate2Index = substS2;
                part.rate = rate;
                roParts[substS1].push_back(part);
                roParts[substS2].push_back(part);
                part.rate = -rate;
                roParts[substP1].push_back(part);
                roParts[substP2].push_back(part);
            }
            else
            {
                throw Exception("Unsupported reaction.");
            }
        }

        // TODO: Fill class atributes by converting vectors to arrays.
    }
    // Collect information about reactions
    ////////////////////////////////////////////////////////////////////////////

}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::AreaSubSolver::~AreaSubSolver()
{
    delete [] D;
    delete [] substIndexes;

    LOG4CXX_DEBUG(log, "~AreaSubSolver()");
    for (int h = 0; h < dataSizeH; h++)
    {
        for (int v = 0; v < dataSizeV; v++)
        {
            for (int s = 0; s < dataSizeS; s++)
            {
                delete[] data[h][v][s];
            }
            delete[] data[h][v];
        }
        delete[] data[h];
    }
    delete[] data;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::AreaSubSolver::solveHorizontalForward()
{
    // This "solve method" is executed firstly, so we must invert layers.
    layersInverted = !layersInverted;

    double timeStep = this->solver->getTimeStep();
    int layerThis = layersInverted ? 2 : 0;
    int i = 0;
    for (int h = 1; h < dataSizeH - 1; h++) // dont calculate boundaries
    {
        double r = startPositionH + stepSizeH * h;
        double ***dataH = data[h];
        for (int v = 1; v < dataSizeV - 1; v++) // dont calculate boundaries
        {
            double **dataHV = dataH[v];
            for (int s = 0; s < dataSizeS; s++)
            {
                double *dataHVS = dataHV[s];

                // Calculate needed coefficients.
                double a = 0.0;
                double b = -2.0 / timeStep;     // b_T
                double c = 0.0;
                double f = -2.0 * dataHVS[layerThis] / timeStep;    // f_T

                // if diffusion?
                if (coordinateSystemIsCartesian)
                {
                    a = c = D[s] / (stepSizeH * stepSizeH);
                    b += - 2.0 * a;
                }
                else    // coordinateSystemIsCylindrical
                {
                    a = D[s] * (r - stepSizeH / 2) / (r * stepSizeH * stepSizeH);
                    b += -2.0 * D[s] / (stepSizeH * stepSizeH);
                    c = D[s] * (r + stepSizeH / 2) / (r * stepSizeH * stepSizeH);
                }
                // f_D is the same for cartesian and cylindrical coordinate systems.
                f += (- D[s] / (stepSizeV * stepSizeV)) * (
                         dataH[v+1][s][layerThis]
                         - 2.0 * dataHVS[layerThis]
                         + dataH[v-1][s][layerThis]
                     );

                // FIXME: Add reactions...

                // And now we are able to calculate layers:
                double tmp = a * data[h - 1][v][s][LAYER_P] + b;
                dataHVS[LAYER_P] = - c / tmp;
                dataHVS[LAYER_Q] = (f - a * data[h - 1][v][s][LAYER_Q]) / tmp;
            }
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::AreaSubSolver::solveHorizontalBackward()
{
    int i = 0;
    for (int h = dataSizeH - 2; h > 0; h--) // dont calculate boundaries
    {
        double ***dataH = data[h];
        for (int v = 1; v < dataSizeV - 1; v++) // dont calculate boundaries
        {
            double **dataHV = dataH[v];
            for (int s = 0; s < dataSizeS; s++)
            {
                double *dataHVS = dataHV[s];

                dataHVS[LAYER_INTERM] =
                    dataHVS[LAYER_P] *
                    data[h + 1][v][s][LAYER_INTERM] +
                    dataHVS[LAYER_Q];
            }
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::AreaSubSolver::solveVerticalForward()
{
    int i = 0;
    for (int h = 0; h < dataSizeH; h++)
    {
        double ***dataH = data[h];
        for (int v = 0; v < dataSizeV; v++)
        {
            double **dataHV = dataH[v];
            for (int s = 0; s < dataSizeS; s++)
            {
                double *dataHVS = dataHV[s];
                //dataHVS[0] += h + v - s;
                // FIXME: Do something.
            }
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::AreaSubSolver::solveVerticalBackward()
{
}


/* ************************************************************************** */
/* ************************************************************************** */
