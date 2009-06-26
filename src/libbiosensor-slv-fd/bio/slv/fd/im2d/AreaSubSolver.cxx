#include "AreaSubSolver.hxx"
#include "Model.hxx"
#include "ModelReaction.hxx"
#include <bio/Logging.hxx>
#include <bio/Exception.hxx>
#include <bio/dm/ConstantSegmentSplit.hxx>
#include <cmath>
#define LOGGER "libbiosensor-slv-fd::im2d::AreaSubSolver: "

/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::AreaSubSolver::AreaSubSolver(
    Solver *solver,
    int positionH,
    int positionV,
    BIO_CFG_NS::StructureAnalyzer* structAnalyzer,
    BIO_SLV_FD_NS::FiniteDifferencesSolverAnalyzer* fdAnalyzer
)
{
    using BIO_DM_NS::ISegmentSplit;
    using BIO_DM_NS::ConstantSegmentSplit;

    LOG_DEBUG(LOGGER << "AreaSubSolver()");

    this->structAnalyzer = structAnalyzer;
    this->fdAnalyzer = fdAnalyzer;

    this->solver = solver;
    this->positionH = positionH;
    this->positionV = positionV;

    this->pointPositionsH = fdAnalyzer->getAxisPartSegmentSplitH(positionH);
    this->pointPositionsV = fdAnalyzer->getAxisPartSegmentSplitV(positionV);


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


    ConstantSegmentSplit* axisPartH = dynamic_cast<ConstantSegmentSplit*> (fdAnalyzer->getAxisPartSegmentSplitH(positionH));
    ConstantSegmentSplit* axisPartV = dynamic_cast<ConstantSegmentSplit*> (fdAnalyzer->getAxisPartSegmentSplitV(positionV));
    if (axisPartH == 0 || axisPartV == 0)
    {
        LOG_ERROR(LOGGER << "Only axis parts of type ConstantAxisPart are now supported.");
        throw Exception("Invalid coonf.");
    }


    this->substanceIndexes = structAnalyzer->getSubstanceIndexesInArea(positionH, positionV);

    this->dataSizeH = axisPartH->getPointCount();
    this->dataSizeV = axisPartV->getPointCount();
    this->dataSizeS = substanceIndexes.size();

    this->stepSizeH = axisPartH->getStepSize(0);
    this->stepSizeV = axisPartV->getStepSize(0);

    this->startPositionH = axisPartH->getStartPosition();
    this->startPositionV = axisPartV->getStartPosition();

    this->dataLayersInverted = false;
    this->targetLayerIndex = getCurrentLayerIndex();


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
                data[h][v][s][getCurrentLayerIndex()] =
                    structAnalyzer->getInitialConcentration(
                        substanceIndexes[s], positionH, positionV
                    )->value();
                data[h][v][s][this->getPreviousLayerIndex()] =
                    data[h][v][s][LAYER_INTERM] =
                        data[h][v][s][LAYER_P] =
                            data[h][v][s][LAYER_Q] =
                                data[h][v][s][LAYER_f_R] = NAN;
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
        typedef std::vector< BIO_XML_NS::model::Reaction* > reactions_vector;
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
                    LOG_ERROR(LOGGER << "For MM reaction both S and P must have diffusion decined in the medium.");
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
                if (rRO->substrate().size() != 2 ||
                        rRO->substrate()[0].coefficient() != 1 ||
                        rRO->substrate()[1].coefficient() != 1
                   )
                {
                    LOG_ERROR(LOGGER << "Implementation of RO reaction is limited to 2 substrates without coefficients");
                    throw Exception("Unsupported reaction.");
                }

                int substS1 = getLocalSubstanceIndex(structAnalyzer->getSubstanceIndex(rRO->substrate()[0].name()));
                int substS2 = getLocalSubstanceIndex(structAnalyzer->getSubstanceIndex(rRO->substrate()[1].name()));
                double rate = structAnalyzer->getSymbol(rRO->rate())->value();
                if (substS1 == -1 || substS2 == -1)
                {
                    LOG_ERROR(LOGGER << "For RO reaction both S1 and S2 must have diffusion defined in the medium.");
                    throw Exception("Unsupported reaction.");
                }

                ReactionROPart part;
                part.substrate1Index = substS1;
                part.substrate2Index = substS2;
                part.rate = rate;
                roParts[substS1].push_back(part);
                roParts[substS2].push_back(part);
                part.rate = -rate;

                for (unsigned p = 0; p < rRO->product().size(); p++)
                {
                    int si = getLocalSubstanceIndex(structAnalyzer->getSubstanceIndex(rRO->product()[p].name()));
                    if (rRO->product()[p].coefficient() != 1)
                    {
                        LOG_ERROR(LOGGER << "Implementation of RO reaction is limited: products must be without coefficients");
                        throw Exception("Unsupported reaction.");
                    }
                    if (si == -1)
                    {
                        LOG_ERROR(LOGGER << "Product of the reaction is not defined in the medium.");
                        throw Exception("Invalid reaction.");
                    }
                    roParts[si].push_back(part);
                }
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
    LOG_DEBUG(LOGGER << "~AreaSubSolver()");


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
    LOG_TRACE(LOGGER << "solveHorizontalForward()...");
    if (dataSizeS == 0)
    {
        // Nothing to solve...
        return;
    }

    // This "solve method" is executed firstly, so we must invert layers.
    dataLayersInverted = !dataLayersInverted;
    targetLayerIndex = LAYER_INTERM;

    dumpData(std::cout, false, "solveHorizontalForward, before calculations");

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

    dumpData(std::cout, false, "solveHorizontalForward, after calculations");

    LOG_TRACE(LOGGER << "solveHorizontalForward()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::AreaSubSolver::solveHorizontalBackward()
{
    LOG_TRACE(LOGGER << "solveHorizontalBackward()...");
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
    dumpData(std::cout, false, "solveHorizontalBackward, after calculations");
    LOG_TRACE(LOGGER << "solveHorizontalBackward()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::AreaSubSolver::solveVerticalForward()
{
    LOG_TRACE(LOGGER << "solveVerticalForward()...");
    if (dataSizeS == 0)
    {
        // Nothing to solve...
        return;
    }

    targetLayerIndex = getCurrentLayerIndex();

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
                double denominator = a * data[h][v - 1][s][LAYER_P] + b;
                dataHVS[LAYER_P] = - c / denominator;
                dataHVS[LAYER_Q] = (f - a * data[h][v - 1][s][LAYER_Q]) / denominator;
            }
        }
    }
    LOG_TRACE(LOGGER << "solveVerticalForward()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::AreaSubSolver::solveVerticalBackward()
{
    LOG_TRACE(LOGGER << "solveVerticalBackward()...");
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
    dumpData(std::cout, true, "solveVerticalBackward, after calculations");
    LOG_TRACE(LOGGER << "solveVerticalBackward()... Done");
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
double BIO_SLV_FD_IM2D_NS::AreaSubSolver::getConcentration(int h, int v, int s)
{
    int sl = getLocalSubstanceIndex(s);
    return (sl == -1) ? NAN : data[h][v][sl][this->getCurrentLayerIndex()];
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::AreaSubSolver::dumpData(std::ostream& out, bool verticalIsInner, std::string message)
{
    return;

    out << "/-----------  AreaSubSolver::dumpData - begin (" << message << ")" << std::endl;
    out << "CurrentLayerIndex=" << getCurrentLayerIndex() << std::endl;
    out
    << (verticalIsInner ? "h\tv" : "v\th")
    << "\ts"
    << (getCurrentLayerIndex() == 0 ? "\tcurrent\tinterm\tprevious" :  "\tprevious\tinterm\tcurrent")
    << "\tp\tq\tf_r"
    << std::endl;
    for (int i = 0; i < (verticalIsInner ? dataSizeH : dataSizeV); i++)
    {
        for (int j = 0; j < (verticalIsInner ? dataSizeV : dataSizeH); j++)
        {
            double **dataHV = verticalIsInner ? data[i][j] : data[j][i];
            for (int s = 0; s < dataSizeS; s++)
            {
                double *dataHVS = dataHV[s];
                out << i << '\t' << j << '\t' << s << '\t'
                << dataHVS[0]  << '\t'
                << dataHVS[1]  << '\t'
                << dataHVS[2]  << '\t'
                << dataHVS[3]  << '\t'
                << dataHVS[4]  << '\t'
                << dataHVS[5]  << std::endl;
            }
        }
    }
    out << "\\-----------  AreaSubSolver::dumpData - end (" << message << ")" << std::endl;
}


/* ************************************************************************** */
/* ************************************************************************** */
int BIO_SLV_FD_IM2D_NS::AreaSubSolver::getSubstanceCount()
{
    return structAnalyzer->getSubstances().size();
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_XML_NS::model::Substance* BIO_SLV_FD_IM2D_NS::AreaSubSolver::getSubstanceConf(int index)
{
    return (this->getLocalSubstanceIndex(index) != -1)
           ? structAnalyzer->getSubstances()[index]
           : 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_DM_NS::ISegmentSplit* BIO_SLV_FD_IM2D_NS::AreaSubSolver::getPointPositionsH()
{
    return this->pointPositionsH;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_DM_NS::ISegmentSplit* BIO_SLV_FD_IM2D_NS::AreaSubSolver::getPointPositionsV()
{
    return this->pointPositionsV;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_DM_NS::ICursor2D* BIO_SLV_FD_IM2D_NS::AreaSubSolver::newGridCursor()
{
    return new Cursor(this);
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::AreaSubSolver::Cursor::Cursor(
    AreaSubSolver* subSolver
)
{
    this->subSolver = subSolver;
    this->sizeH = subSolver->getPointPositionsH()->getPointCount();
    this->sizeV = subSolver->getPointPositionsV()->getPointCount();
    this->currentH = 0;
    this->currentV = 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::AreaSubSolver::Cursor::~Cursor()
{
    // Nothing to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::AreaSubSolver::Cursor::left()
{
    --currentH;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::AreaSubSolver::Cursor::right()
{
    currentH++;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::AreaSubSolver::Cursor::top()
{
    currentV--;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::AreaSubSolver::Cursor::down()
{
    currentV++;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::AreaSubSolver::Cursor::rowStart()
{
    currentH = 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::AreaSubSolver::Cursor::rowEnd()
{
    currentH = sizeH - 1;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::AreaSubSolver::Cursor::colStart()
{
    currentV = 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::AreaSubSolver::Cursor::colEnd()
{
    currentV = sizeV - 1;
}


/* ************************************************************************** */
/* ************************************************************************** */
bool BIO_SLV_FD_IM2D_NS::AreaSubSolver::Cursor::isValid()
{
    return currentH >= 0 && currentH < sizeH && currentV >= 0 && currentV < sizeV;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_DM_NS::IConcentrations* BIO_SLV_FD_IM2D_NS::AreaSubSolver::Cursor::getConcentrations()
{
    return isValid() ? this : 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_SLV_FD_IM2D_NS::AreaSubSolver::Cursor::getConcentration(int substanceNr)
{
    return subSolver->getConcentration(
               currentH,
               currentV,
               substanceNr
           );
}


/* ************************************************************************** */
/* ************************************************************************** */
