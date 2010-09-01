#include "AreaSubSolver.hxx"
#include "Model.hxx"
#include "ModelReaction.hxx"
#include "ReactionMacro.hxx"
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
    LOG_DEBUG(LOGGER << "AreaSubSolver()...");

    this->structAnalyzer = structAnalyzer;
    this->fdAnalyzer = fdAnalyzer;

    this->solver = solver;
    this->positionH = positionH;
    this->positionV = positionV;

    LOG_DEBUG(LOGGER << "AreaSubSolver()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::AreaSubSolver::initialize()
{
    using BIO_DM_NS::ISegmentSplit;
    using BIO_DM_NS::ConstantSegmentSplit;

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
    data = createData();
    //
    //  Create data structures
    ////////////////////////////////////////////////////////////////////////////


    ////////////////////////////////////////////////////////////////////////////
    // Collect information about diffusions
    D_h = new double[dataSizeS];
    D_v = new double[dataSizeS];
    for (int s = 0; s < dataSizeS; s++)
    {
        D_h[s] = structAnalyzer->getDiffusionCoef(substanceIndexes[s], positionH, positionV, true);
        D_v[s] = structAnalyzer->getDiffusionCoef(substanceIndexes[s], positionH, positionV, false);
        LOG_DEBUG(LOGGER
                  << "Diffusion coefficients for"
                  << " substance=" << structAnalyzer->getSubstances()[substanceIndexes[s]]->name()
                  << " are: D_h=" << D_h[s] << " D_v=" << D_v[s]
                 );
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
                //  TODO: Implement normal checking here...
                //
                //if (rRO->substrate().size() != 2 ||
                //        rRO->substrate()[0].coefficient() != 1 ||
                //        rRO->substrate()[1].coefficient() != 1
                //   )
                //{
                //    LOG_ERROR(LOGGER << "Implementation of RO reaction is limited to 2 substrates without coefficients");
                //    throw Exception("Unsupported reaction.");
                //}
                bool haveTwo = rRO->substrate().size() == 2;

                int substS1 =           getLocalSubstanceIndex(structAnalyzer->getSubstanceIndex(rRO->substrate()[0].name()))     ;
                int substS2 = haveTwo ? getLocalSubstanceIndex(structAnalyzer->getSubstanceIndex(rRO->substrate()[1].name())) : -1;
                double rate = structAnalyzer->getSymbol(rRO->rate())->value();
                if (substS1 == -1 || (haveTwo && substS2 == -1))
                {
                    LOG_ERROR(LOGGER << "For RO reaction both S1 and S2 must have diffusion defined in the medium.");
                    throw Exception("Unsupported reaction.");
                }

                ReactionROPart part;
                part.substrate1Index = substS1;
                part.substrate2Index = substS2;
                part.rate = rate;

                roParts[substS1].push_back(part);
                if (haveTwo)
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
    LOG_TRACE(LOGGER << "~AreaSubSolver()");


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

    delete [] D_h;
    delete [] D_v;

}

void BIO_SLV_FD_IM2D_NS::AreaSubSolver::destroy()
{
    deleteData(data);
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

    // Make bject properties as local variables, for performance...
    static const int LAYER_INTERM = this->LAYER_INTERM;
    static const int LAYER_P = this->LAYER_P;
    static const int LAYER_Q = this->LAYER_Q;
    double ****data = this->data;
    int dataSizeH = this->dataSizeH;
    int dataSizeV = this->dataSizeV;
    int dataSizeS = this->dataSizeS;
    double stepSizeH = this->stepSizeH;
    double stepSizeV = this->stepSizeV;
    double startPositionH = this->startPositionH;
    bool coordinateSystemIsCartesian = this->coordinateSystemIsCartesian;
    double *D_h = this->D_h;
    double *D_v = this->D_v;
    ReactionMMPart **reactionsMM = this->reactionsMM;
    int *reactionsMMPartCounts = this->reactionsMMPartCounts;
    ReactionROPart **reactionsRO = this->reactionsRO;
    int *reactionsROPartCounts = this->reactionsROPartCounts;



    // This "solve method" is executed firstly, so we must invert layers.
    dataLayersInverted = !dataLayersInverted;
    targetLayerIndex = LAYER_INTERM;

    dumpData(std::cout, false, "solveHorizontalForward, before calculations");

    double exprTimeStep = -2.0 / this->solver->getTimeStep();   // "2" stands here for the HALF step (1/(t/2)).
    int layerPrev = this->getPreviousLayerIndex();

    for (int h = 1; h < dataSizeH - 1; h++) // dont calculate boundaries
    {
        double r = startPositionH + stepSizeH * h;
        double cylAx = (r - stepSizeH * 0.5) / (r * stepSizeH * stepSizeH);
        double cylCx = (r + stepSizeH * 0.5) / (r * stepSizeH * stepSizeH);
        double cylBDiffx = -2.0 / (stepSizeH * stepSizeH);
        double exprFx = -1.0 / (stepSizeV * stepSizeV);
        double ***dataH = data[h];
        for (int v = 1; v < dataSizeV - 1; v++) // dont calculate boundaries
        {
            double **dataHV = dataH[v];
            for (int s = 0; s < dataSizeS; s++)
            {
                double *dataHVS = dataHV[s];
                double *dataHVS_L = data[h-1][v][s];
                double *dataHVS_T = dataH[v-1][s];
                double *dataHVS_B = dataH[v+1][s];
                double D_hs = D_h[s];
                double D_vs = D_v[s];
                double cylA = cylAx * D_hs;
                double cylC = cylCx * D_hs;
                double cylBDiff = cylBDiffx * D_hs;
                double exprF = exprFx * D_vs;

                //
                //  Time part of the coefficients (b+=b_T, f+=f_T)
                //
                double a = 0.0;
                double b = exprTimeStep;                         // b_T
                double c = 0.0;
                double f = exprTimeStep * dataHVS[layerPrev];    // f_T


                //
                //  Diffusion part (a=a_D, b+=b_D, c=c_D, f+=f_D)
                //
                if (coordinateSystemIsCartesian)
                {
                    a = c = D_h[s] / (stepSizeH * stepSizeH);
                    b += - 2.0 * a;
                }
                else    // coordinateSystemIsCylindrical
                {
                    a = cylA;
                    b += cylBDiff;
                    c = cylC;
                }
                // f_D is the same for cartesian and cylindrical coordinate systems.
                f += exprF * (
                         dataHVS_B[layerPrev]
                         - 2.0 * dataHVS[layerPrev]
                         + dataHVS_T[layerPrev]
                     );

                //
                //  Reaction part (f+=f_R)
                //  f_R is saved for reuse in the solveVerticalForward.
                //
                double f_R = 0.0;
                AREA_SUBSOLVER_REACTION_MACRO(
                    f_R, layerPrev, dataHV,
                    reactionsMM[s], reactionsMMPartCounts[s],
                    reactionsRO[s], reactionsROPartCounts[s]
                )
                f += f_R;

                //
                // And now we are able to calculate layers:
                //
                double denominator = 1.0 / (a * dataHVS_L[LAYER_P] + b);
                dataHVS[LAYER_P] = - c * denominator;
                dataHVS[LAYER_Q] = (f - a * dataHVS_L[LAYER_Q]) * denominator;
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

    static const int LAYER_P = this->LAYER_P;
    static const int LAYER_Q = this->LAYER_Q;
    static const int LAYER_INTERM = this->LAYER_INTERM;
    double ****data = this->data;
    int dataSizeH = this->dataSizeH;
    int dataSizeV = this->dataSizeV;
    int dataSizeS = this->dataSizeS;

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

    // Make bject properties as local variables, for performance...
    static const int LAYER_INTERM = this->LAYER_INTERM;
    static const int LAYER_P = this->LAYER_P;
    static const int LAYER_Q = this->LAYER_Q;
    double ****data = this->data;
    int dataSizeH = this->dataSizeH;
    int dataSizeV = this->dataSizeV;
    int dataSizeS = this->dataSizeS;
    double stepSizeH = this->stepSizeH;
    double stepSizeV = this->stepSizeV;
    double startPositionH = this->startPositionH;
    bool coordinateSystemIsCartesian = this->coordinateSystemIsCartesian;
    double *D_h = this->D_h;
    double *D_v = this->D_v;

    targetLayerIndex = getCurrentLayerIndex();

    //double timeStep = this->solver->getTimeStep();
    double exprTimeStep = -2.0 / this->solver->getTimeStep();
    double exprStepSizeV = 1.0 / (stepSizeV * stepSizeV);
    double exprStepSizeH = 1.0 / (stepSizeH * stepSizeH);

    for (int h = 1; h < dataSizeH - 1; h++) // dont calculate boundaries
    {
        double r = startPositionH + stepSizeH * h;
        double exprStepSizeHr = exprStepSizeH / r;
        double exprRHalfNext = r + stepSizeH * 0.5;
        double exprRHalfPrev = r - stepSizeH * 0.5;
        double ***dataH = data[h];
        for (int v = 1; v < dataSizeV - 1; v++) // dont calculate boundaries
        {
            double **dataHV = dataH[v];
            for (int s = 0; s < dataSizeS; s++)
            {
                double *dataHVS = dataHV[s];
                double D_hs = D_h[s];
                double D_vs = D_v[s];

                //
                //  Time part of the coefficients (b+=b_T, f+=f_T+f_R)
                //  Saved reaction part is also added here.
                //
                double a = 0.0;
                double b = exprTimeStep;                                // b_T
                double c = 0.0;
                double f = exprTimeStep * dataHVS[LAYER_INTERM];        // f_T


                //
                //  Diffusion part (a=a_D, b+=b_D, c=c_D, f+=f_D)
                //
                a = c = D_vs * exprStepSizeV;
                b += - 2.0 * a;
                if (coordinateSystemIsCartesian)
                {
                    f += - D_hs * exprStepSizeH * (
                             data[h+1][v][s][LAYER_INTERM]
                             - 2.0 * dataHVS[LAYER_INTERM]
                             + data[h-1][v][s][LAYER_INTERM]
                         );
                }
                else    // coordinateSystemIsCylindrical
                {
                    f += - D_hs * exprStepSizeHr * (
                             + exprRHalfNext * data[h+1][v][s][LAYER_INTERM]
                             - 2.0 * r * dataHVS[LAYER_INTERM]
                             + exprRHalfPrev * data[h-1][v][s][LAYER_INTERM]
                         );
                }

                //
                //  Reaction part (f+=f_R)
                //
                double f_R = 0.0;
                AREA_SUBSOLVER_REACTION_MACRO(
                    f_R, LAYER_INTERM, dataHV,
                    reactionsMM[s], reactionsMMPartCounts[s],
                    reactionsRO[s], reactionsROPartCounts[s]
                )
                f += f_R;          // f_R

                //
                // And now we are able to calculate layers:
                //
                double denominator = 1.0 / (a * data[h][v - 1][s][LAYER_P] + b);
                dataHVS[LAYER_P] = - c * denominator;
                dataHVS[LAYER_Q] = (f - a * data[h][v - 1][s][LAYER_Q]) * denominator;
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

    static const int LAYER_P = this->LAYER_P;
    static const int LAYER_Q = this->LAYER_Q;
    double ****data = this->data;
    int dataSizeH = this->dataSizeH;
    int dataSizeV = this->dataSizeV;
    int dataSizeS = this->dataSizeS;

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
BIO_SLV_FD_IM2D_NS::IAreaEdgeData* BIO_SLV_FD_IM2D_NS::AreaSubSolver::getEdgeData(
    int substance,
    bool horizontal,
    bool start,
    bool targetLayer
)
{
    EdgeData* edge;
    if (targetLayer)
    {
        edge = new EdgeData(
            this,
            getLocalSubstanceIndex(substance),
            horizontal,
            start
        );
    }
    else
    {
        edge = new EdgeDataPrevLayer(
            this,
            getLocalSubstanceIndex(substance),
            horizontal,
            start
        );
    }
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
    this->forward = start;
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

BIO_SLV_FD_IM2D_NS::AreaSubSolver::EdgeData::~EdgeData()
{
    if (data0)
        delete [] data0;
    if (data1)
        delete [] data1;
}

int BIO_SLV_FD_IM2D_NS::AreaSubSolver::EdgeData::getSize()
{
    return size;
}

double BIO_SLV_FD_IM2D_NS::AreaSubSolver::EdgeData::getStepSize()
{
    return stepSize;
}

bool BIO_SLV_FD_IM2D_NS::AreaSubSolver::EdgeData::isForward()
{
    return forward;
}

void BIO_SLV_FD_IM2D_NS::AreaSubSolver::EdgeData::setP0(int index, double value)
{
    data0[index][LAYER_P] = value;
}

double BIO_SLV_FD_IM2D_NS::AreaSubSolver::EdgeData::getP0(int index)
{
    return data0[index][LAYER_P];
}

double BIO_SLV_FD_IM2D_NS::AreaSubSolver::EdgeData::getP1(int index)
{
    return data1[index][LAYER_P];
}

void BIO_SLV_FD_IM2D_NS::AreaSubSolver::EdgeData::setQ0(int index, double value)
{
    data0[index][LAYER_Q] = value;
}

double BIO_SLV_FD_IM2D_NS::AreaSubSolver::EdgeData::getQ0(int index)
{
    return data0[index][LAYER_Q];
}

double BIO_SLV_FD_IM2D_NS::AreaSubSolver::EdgeData::getQ1(int index)
{
    return data1[index][LAYER_Q];
}

void BIO_SLV_FD_IM2D_NS::AreaSubSolver::EdgeData::setC0(int index, double value)
{
    data0[index][solver->getTargetLayerIndex()] = value;
}

double BIO_SLV_FD_IM2D_NS::AreaSubSolver::EdgeData::getC0(int index)
{
    return data0[index][solver->getTargetLayerIndex()];
}

double BIO_SLV_FD_IM2D_NS::AreaSubSolver::EdgeData::getC1(int index)
{
    return data1[index][solver->getTargetLayerIndex()];
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::AreaSubSolver::EdgeDataPrevLayer::EdgeDataPrevLayer(
    AreaSubSolver* solver,
    int substance,
    bool horizontal,
    bool start
) : EdgeData(solver, substance, horizontal, start)
{
    //  Nothing more.
}

BIO_SLV_FD_IM2D_NS::AreaSubSolver::EdgeDataPrevLayer::~EdgeDataPrevLayer()
{
    //  Nothing.
}

void BIO_SLV_FD_IM2D_NS::AreaSubSolver::EdgeDataPrevLayer::setP0(int index, double value)
{
    throw BIO_NS::Exception("AreaSubSolver::EdgeDataPrevLayer is readonly");
}

double BIO_SLV_FD_IM2D_NS::AreaSubSolver::EdgeDataPrevLayer::getP0(int index)
{
    throw BIO_NS::Exception("AreaSubSolver::EdgeDataPrevLayer only supports reading of concentrations");
}

double BIO_SLV_FD_IM2D_NS::AreaSubSolver::EdgeDataPrevLayer::getP1(int index)
{
    throw BIO_NS::Exception("AreaSubSolver::EdgeDataPrevLayer only supports reading of concentrations");
}

void BIO_SLV_FD_IM2D_NS::AreaSubSolver::EdgeDataPrevLayer::setQ0(int index, double value)
{
    throw BIO_NS::Exception("AreaSubSolver::EdgeDataPrevLayer is readonly");
}

double BIO_SLV_FD_IM2D_NS::AreaSubSolver::EdgeDataPrevLayer::getQ0(int index)
{
    throw BIO_NS::Exception("AreaSubSolver::EdgeDataPrevLayer only supports reading of concentrations");
}

double BIO_SLV_FD_IM2D_NS::AreaSubSolver::EdgeDataPrevLayer::getQ1(int index)
{
    throw BIO_NS::Exception("AreaSubSolver::EdgeDataPrevLayer only supports reading of concentrations");
}

void BIO_SLV_FD_IM2D_NS::AreaSubSolver::EdgeDataPrevLayer::setC0(int index, double value)
{
    throw BIO_NS::Exception("AreaSubSolver::EdgeDataPrevLayer is readonly");
}

double BIO_SLV_FD_IM2D_NS::AreaSubSolver::EdgeDataPrevLayer::getC0(int index)
{
    return data0[index][solver->getPreviousLayerIndex()];
}

double BIO_SLV_FD_IM2D_NS::AreaSubSolver::EdgeDataPrevLayer::getC1(int index)
{
    return data1[index][solver->getPreviousLayerIndex()];
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
void BIO_SLV_FD_IM2D_NS::AreaSubSolver::setConcentration(int h, int v, int s, double c)
{
    int sl = getLocalSubstanceIndex(s);
    if (sl != -1)
    {
        data[h][v][sl][this->getCurrentLayerIndex()] = c;

        //  I hope this is not needed.
        //  If needed, this must be fixed in the bound conditions also.
        //data[h][v][sl][this->getPreviousLayerIndex()] = c;
    }
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
double**** BIO_SLV_FD_IM2D_NS::AreaSubSolver::createData()
{
    double**** data = new double***[dataSizeH];
    for (int h = 0; h < dataSizeH; h++)
    {
        data[h] = new double**[dataSizeV];
        for (int v = 0; v < dataSizeV; v++)
        {
            data[h][v] = createDataPoint();
        }
    }
    return data;
}

void BIO_SLV_FD_IM2D_NS::AreaSubSolver::deleteData(double**** data)
{
    for (int h = 0; h < dataSizeH; h++)
    {
        for (int v = 0; v < dataSizeV; v++)
        {
            deleteDataPoint(data[h][v]);
        }
        delete[] data[h];
    }
    delete[] data;
}


/* ************************************************************************** */
/* ************************************************************************** */
double** BIO_SLV_FD_IM2D_NS::AreaSubSolver::createDataPoint()
{
    double** point = new double*[dataSizeS];
    for (int s = 0; s < dataSizeS; s++)
    {
        point[s] = new double[LAYER_COUNT];

        // Apply initial conditions.
        point[s][0] = point[s][1] = point[s][2] =
                                        structAnalyzer->getInitialConcentration(
                                            substanceIndexes[s], positionH, positionV
                                        )->value();

        point[s][this->getPreviousLayerIndex()] =
            point[s][LAYER_INTERM] =
                point[s][LAYER_P] =
                    point[s][LAYER_Q] = NAN;
    }
    return point;
}

void BIO_SLV_FD_IM2D_NS::AreaSubSolver::deleteDataPoint(double** point)
{
    for (int s = 0; s < dataSizeS; s++)
    {
        delete[] point[s];
    }
    delete[] point;
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
) :
        BIO_DM_NS::AbstractCursor2D(
            subSolver->getPointPositionsH()->getPointCount(),
            subSolver->getPointPositionsV()->getPointCount()
        )
{
    this->subSolver = subSolver;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::AreaSubSolver::Cursor::~Cursor()
{
    // Nothing to do here.
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
void BIO_SLV_FD_IM2D_NS::AreaSubSolver::Cursor::setConcentration(int substanceNr, double concentration)
{
    subSolver->setConcentration(
        currentH,
        currentV,
        substanceNr,
        concentration
    );
}


/* ************************************************************************** */
/* ************************************************************************** */
