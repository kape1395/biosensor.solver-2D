#include "AreaSubSolver.hxx"
#include "Model.hxx"
#include "ModelReaction.hxx"
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
    using BIO_XML_NS::model::solver::Axis;
    using BIO_XML_NS::model::solver::ConstantAxisPart;

    LOG4CXX_DEBUG(log, "AreaSubSolver()");
    this->solver = solver;
    this->positionH = positionH;
    this->positionV = positionV;


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

    //
    //  NOTE:   The cineticts is modelled only for substances, for which
    //          the diffusion is defined.
    //


    ConstantAxisPart* axisPartH = dynamic_cast<ConstantAxisPart*> (fdAnalyzer->getAxisPartH(positionH));
    ConstantAxisPart* axisPartV = dynamic_cast<ConstantAxisPart*> (fdAnalyzer->getAxisPartV(positionV));
    if (axisPartH == 0 || axisPartV == 0)
    {
        LOG4CXX_ERROR(log, "Only axis parts of type ConstantAxisPart are now supported.");
        throw Exception("Invalid coonf.");
    }


    this->substanceIndexes = structAnalyzer->getSubstanceIndexesInArea(positionH, positionV);

    this->dataSizeH = axisPartH->stepCount() + 1;
    this->dataSizeV = axisPartV->stepCount() + 1;
    this->dataSizeS = substanceIndexes.size();

    this->stepSizeH = (structAnalyzer->getSymbol(axisPartH->to())->value() -
                       structAnalyzer->getSymbol(axisPartH->from())->value()) /
                      axisPartH->stepCount();

    this->stepSizeV = (structAnalyzer->getSymbol(axisPartV->to())->value() -
                       structAnalyzer->getSymbol(axisPartV->from())->value()) /
                      axisPartV->stepCount();

    this->startPositionH = structAnalyzer->getSymbol(axisPartH->from())->value();
    this->startPositionV = structAnalyzer->getSymbol(axisPartV->from())->value();

    this->dataLayersInverted = false;


    ////////////////////////////////////////////////////////////////////////////
    //  Create data structures
    //
    data = new double***[dataSizeH];
    for (int h = 0; h < dataSizeH; h++)
    {
        data[h] = new double**[dataSizeV];
        for (int v = 0; v < dataSizeV; v++)
        {
            data[h][v] = new double*[dataSizeS];
            for (int s = 0; s < dataSizeS; s++)
            {
                data[h][v][s] = new double[6];

                // Apply initial conditions.
                data[h][v][s][getCurrentLayerIndex()] = structAnalyzer->getInitialConcentration(
                                                            substanceIndexes[s], positionH, positionV
                                                        )->value();
            }
        }
    }
    //
    //  Create data structures
    ////////////////////////////////////////////////////////////////////////////


    ////////////////////////////////////////////////////////////////////////////
    // Collect information about diffusions
    D = new double[dataSizeS];
    for (int s = 0; s < dataSizeS; s++)
    {
        D[s] = structAnalyzer->getDiffusion(substanceIndexes[s], positionH, positionV)->value();
    }
    // Collect information about diffusions
    ////////////////////////////////////////////////////////////////////////////


    ////////////////////////////////////////////////////////////////////////////
    // Collect information about reactions
    {
        typedef std::vector< BIO_XML_NS::model::MediumReaction* > reactions_vector;
        typedef BIO_XML_NS::model::reaction::MichaelisMenten R_MM;
        typedef BIO_XML_NS::model::reaction::ReductionOxidation R_RO;

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
                int substS = getLocalSubstanceIndex(structAnalyzer->getSubstanceIndex(rMM->substrate()));
                int substP = getLocalSubstanceIndex(structAnalyzer->getSubstanceIndex(rMM->product()));
                if (substS == -1 || substP == -1)
                {
                    LOG4CXX_ERROR(log, "For MM reaction both S and P must have diffusion decined in the medium.");
                    throw Exception("Unsupported reaction.");
                }

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
                if (rRO->substrate().size() != 2 || rRO->product().size() != 2 ||
                        rRO->substrate()[0].coefficient() != 1 ||
                        rRO->substrate()[1].coefficient() != 1 ||
                        rRO->product()[0].coefficient() != 1 ||
                        rRO->product()[1].coefficient() != 1
                   )
                {
                    LOG4CXX_ERROR(log, "Implementation of RO reaction is limited: 2S + 2P without coefficients");
                    throw Exception("Unsupported reaction.");
                }

                int substS1 = getLocalSubstanceIndex(structAnalyzer->getSubstanceIndex(rRO->substrate()[0].name()));
                int substS2 = getLocalSubstanceIndex(structAnalyzer->getSubstanceIndex(rRO->substrate()[1].name()));
                int substP1 = getLocalSubstanceIndex(structAnalyzer->getSubstanceIndex(rRO->product()[0].name()));
                int substP2 = getLocalSubstanceIndex(structAnalyzer->getSubstanceIndex(rRO->product()[1].name()));
                double rate = structAnalyzer->getSymbol(rRO->rate())->value();
                if (substS1 == -1 || substS2 == -1)
                {
                    LOG4CXX_ERROR(log, "For RO reaction both S1 and S2 must have diffusion decined in the medium.");
                    throw Exception("Unsupported reaction.");
                }

                ReactionROPart part;
                part.substrate1Index = substS1;
                part.substrate2Index = substS2;
                part.rate = rate;
                roParts[substS1].push_back(part);
                roParts[substS2].push_back(part);
                part.rate = -rate;
                if (substP1 != -1)
                    roParts[substP1].push_back(part);
                if (substP2 != -1)
                    roParts[substP2].push_back(part);
            }
            else
            {
                throw Exception("Unsupported reaction.");
            }
        }

        //
        //  Convert vectors to the arrays.
        //
        reactionsMM = new ReactionMMPart*[dataSizeS];
        reactionsRO = new ReactionROPart*[dataSizeS];
        reactionsMMPartCounts = new int[dataSizeS];
        reactionsROPartCounts = new int[dataSizeS];
        for (int s = 0; s < dataSizeS; s++)
        {
            reactionsMMPartCounts[s] = mmParts[s].size();
            reactionsMM[s] = new ReactionMMPart[reactionsMMPartCounts[s]];
            for (int i = 0; i < reactionsMMPartCounts[s]; i++)
            {
                reactionsMM[s][i] = mmParts[s][i];
            }
            reactionsROPartCounts[s] = roParts[s].size();
            reactionsRO[s] = new ReactionROPart[reactionsROPartCounts[s]];
            for (int i = 0; i < reactionsROPartCounts[s]; i++)
            {
                reactionsRO[s][i] = roParts[s][i];
            }
        }
        delete [] mmParts;
        delete [] roParts;
    }
    // Collect information about reactions
    ////////////////////////////////////////////////////////////////////////////

}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::AreaSubSolver::~AreaSubSolver()
{
    LOG4CXX_DEBUG(log, "~AreaSubSolver()");


    for (std::vector<EdgeData*>::iterator e = edges.begin(); e < edges.end(); e++)
    {
        delete *e;
    }
    edges.clear();


    for (int s = 0; s < dataSizeS; s++)
    {
        delete [] reactionsMM[s];
        delete [] reactionsRO[s];
    }
    delete [] reactionsMM;
    delete [] reactionsRO;
    delete [] reactionsMMPartCounts;
    delete [] reactionsROPartCounts;

    delete [] D;

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
/**
 *  Some additional documentation.
 */
void BIO_SLV_FD_IM2D_NS::AreaSubSolver::solveHorizontalForward()
{
    if (dataSizeS == 0)
    {
        // Nothing to solve...
        return;
    }

    // This "solve method" is executed firstly, so we must invert layers.
    dataLayersInverted = !dataLayersInverted;

    double timeStep = this->solver->getTimeStep();
    int layerPrev = this->getPreviousLayerIndex();

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

                //
                //  Time part of the coefficients (b+=b_T, f+=f_T)
                //
                double a = 0.0;
                double b = -2.0 / timeStep;                         // b_T
                double c = 0.0;
                double f = -2.0 * dataHVS[layerPrev] / timeStep;    // f_T


                //
                //  Diffusion part (a=a_D, b+=b_D, c=c_D, f+=f_D)
                //
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
                         dataH[v+1][s][layerPrev]
                         - 2.0 * dataHVS[layerPrev]
                         + dataH[v-1][s][layerPrev]
                     );

                //
                //  Reaction part (f+=f_R)
                //  f_R is saved for reuse in the solveVerticalForward.
                //
                double f_R = 0.0;
                for (int r = 0; r < reactionsMMPartCounts[s]; r++)
                {
                    ReactionMMPart mm = reactionsMM[s][r];
                    f_R += (mm.V_max * dataHV[mm.substrateIndex][layerPrev])
                           / (mm.K_M + dataHV[mm.substrateIndex][layerPrev]);
                }
                for (int r = 0; r < reactionsROPartCounts[s]; r++)
                {
                    ReactionROPart ro = reactionsRO[s][r];
                    f_R += ro.rate
                           * dataHV[ro.substrate1Index][layerPrev]
                           * dataHV[ro.substrate2Index][layerPrev];
                }
                f += dataHVS[LAYER_f_R] = f_R;

                //
                // And now we are able to calculate layers:
                //
                double denominator = a * data[h - 1][v][s][LAYER_P] + b;
                dataHVS[LAYER_P] = - c / denominator;
                dataHVS[LAYER_Q] = (f - a * data[h - 1][v][s][LAYER_Q]) / denominator;
            }
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::AreaSubSolver::solveHorizontalBackward()
{
    if (dataSizeS == 0)
    {
        // Nothing to solve...
        return;
    }

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
    if (dataSizeS == 0)
    {
        // Nothing to solve...
        return;
    }

    double timeStep = this->solver->getTimeStep();

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

                //
                //  Time part of the coefficients (b+=b_T, f+=f_T+f_R)
                //  Saved reaction part is also added here.
                //
                double a = 0.0;
                double b = -2.0 / timeStep;                             // b_T
                double c = 0.0;
                double f = (-2.0 * dataHVS[LAYER_INTERM] / timeStep)    // f_T
                           + dataHVS[LAYER_f_R];                        // f_R


                //
                //  Diffusion part (a=a_D, b+=b_D, c=c_D, f+=f_D)
                //
                a = c = D[s] / (stepSizeV * stepSizeV);
                b += - 2.0 * a;
                if (coordinateSystemIsCartesian)
                {
                    f += (- D[s] / (stepSizeH * stepSizeH)) * (
                             data[h+1][v][s][LAYER_INTERM]
                             - 2.0 * dataHVS[LAYER_INTERM]
                             + data[h-1][v][s][LAYER_INTERM]
                         );
                }
                else    // coordinateSystemIsCylindrical
                {
                    f += (- D[s] / (r * stepSizeH * stepSizeH)) * (
                             + (r + stepSizeH / 2) * data[h+1][v][s][LAYER_INTERM]
                             - 2.0 * r * dataHVS[LAYER_INTERM]
                             + (r - stepSizeH / 2) * data[h-1][v][s][LAYER_INTERM]
                         );
                }

                //
                //  Reaction part (f+=f_R) is alredy added together with f_T)
                //

                //
                // And now we are able to calculate layers:
                //
                double denominator = a * data[h - 1][v][s][LAYER_P] + b;
                dataHVS[LAYER_P] = - c / denominator;
                dataHVS[LAYER_Q] = (f - a * data[h - 1][v][s][LAYER_Q]) / denominator;
            }
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::AreaSubSolver::solveVerticalBackward()
{
    if (dataSizeS == 0)
    {
        // Nothing to solve...
        return;
    }

    int layerCurrent = getCurrentLayerIndex();
    for (int h = 1; h < dataSizeH - 1; h++) // dont calculate boundaries
    {
        double ***dataH = data[h];
        for (int v = dataSizeV - 2; v > 0; v--) // dont calculate boundaries
        {
            double **dataHV = dataH[v];
            for (int s = 0; s < dataSizeS; s++)
            {
                double *dataHVS = dataHV[s];

                dataHVS[layerCurrent] =
                    dataHVS[LAYER_P] *
                    data[h][v + 1][s][layerCurrent] +
                    dataHVS[LAYER_Q];
            }
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
int BIO_SLV_FD_IM2D_NS::AreaSubSolver::getLocalSubstanceIndex(int globalSubstanceIndex)
{
    for (int i = 0; i < dataSizeS; i++)
    {
        if (substanceIndexes[i] == globalSubstanceIndex)
        {
            return i;
        }
    }
    return -1;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::AreaSubSolver::EdgeData* BIO_SLV_FD_IM2D_NS::AreaSubSolver::getEdgeData(
    int substance,
    bool horizontal,
    bool start
)
{
    EdgeData* edge = new EdgeData(
        this,
        getLocalSubstanceIndex(substance),
        horizontal,
        start
    );
    edges.push_back(edge);
    return edge;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::AreaSubSolver::EdgeData::EdgeData(
    AreaSubSolver* solver,
    int substance,
    bool horizontal,
    bool start
)
{
    this->solver = solver;
    if (horizontal)
    {
        size = solver->dataSizeH;
        stepSize = solver->stepSizeV;
        data0 = new double*[size];
        data1 = new double*[size];
        for (int i = 0; i < size; i++)
        {
            data0[i] = solver->data[i][start ? 0 : solver->dataSizeV - 1][substance];
            data1[i] = solver->data[i][start ? 1 : solver->dataSizeV - 2][substance];
        }
    }
    else // vertical
    {
        size = solver->dataSizeV;
        stepSize = solver->stepSizeH;
        data0 = new double*[size];
        data1 = new double*[size];
        for (int i = 0; i < size; i++)
        {
            data0[i] = solver->data[start ? 0 : solver->dataSizeH - 1][i][substance];
            data1[i] = solver->data[start ? 1 : solver->dataSizeH - 2][i][substance];
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::AreaSubSolver::EdgeData::~EdgeData()
{
    delete [] data0;
    delete [] data1;
}


/* ************************************************************************** */
/* ************************************************************************** */
