#include "BoundAnalyzer.hxx"
#include "../Exception.hxx"
#include "../Logging.hxx"
#define LOGGER "libbiosensor::BoundAnalyzer: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_CFG_NS::BoundAnalyzer::BoundAnalyzer(
    StructureAnalyzer *structAnalyzer
)
{
    typedef BIO_XML_NS::model::Model::bound_iterator bIt;
    typedef BIO_XML_NS::model::Bound::substance_iterator bsIt;

    this->structAnalyzer = structAnalyzer;
    this->sizeH = structAnalyzer->getPointsH().size() - 1;
    this->sizeV = structAnalyzer->getPointsV().size() - 1;
    this->sizeS = structAnalyzer->getSubstances().size();

    boundSubstances = new BoundSubstanceInfo* ***[sizeH];
    for (int h = 0; h < sizeH; h++)
    {
        boundSubstances[h] = new BoundSubstanceInfo* **[sizeV];
        for (int v = 0; v < sizeV; v++)
        {
            boundSubstances[h][v] = new BoundSubstanceInfo* *[sizeS];
            for (int s = 0; s < sizeS; s++)
            {
                boundSubstances[h][v][s] = new BoundSubstanceInfo* [4];    // 4 sides
                for (int i = 0; i < 4; i++)
                {
                    boundSubstances[h][v][s][i] = new BoundSubstanceInfo();
                }
            }
        }
    }


    /////////////////////////////////////////////////////////////
    //  Get all bound specs from the model.
    //
    LOG_INFO(LOGGER << "Applying all provided bound conditions...");
    BIO_XML_NS::model::Model *model = structAnalyzer->getConfig();
    for (bIt b = model->bound().begin(); b < model->bound().end(); b++)
    {
        ////////////////////////////////
        //  Get bound position and perform some checks
        //
        StructureAnalyzer::Axis parallelAxis;
        StructureAnalyzer::Axis perpendicularAxis;

        if (structAnalyzer->isPointInAxis(StructureAnalyzer::HORIZONTAL, b->at()))
        {
            parallelAxis = StructureAnalyzer::VERTICAL;
            perpendicularAxis = StructureAnalyzer::HORIZONTAL;
        }
        else if (structAnalyzer->isPointInAxis(StructureAnalyzer::VERTICAL, b->at()))
        {
            parallelAxis = StructureAnalyzer::HORIZONTAL;
            perpendicularAxis = StructureAnalyzer::VERTICAL;
        }
        else
            throw Exception("Symbols, used to define bounds are not points in the axes");

        if (!b->from().present() || !b->to().present())
            throw Exception("For 2D model all 'at', 'from' and 'to' must be specified");

        if (!structAnalyzer->isPointInAxis(parallelAxis, b->from().get()) ||
                !structAnalyzer->isPointInAxis(parallelAxis, b->to().get()))
            throw Exception("Bound`s 'from' and 'to' must be on axis, that perpendicular to axis on which is 'at'.");

        int boundAt     = structAnalyzer->getPointIndexInAxis(perpendicularAxis, b->at());
        int boundFrom   = structAnalyzer->getPointIndexInAxis(parallelAxis, b->from().get());
        int boundTo     = structAnalyzer->getPointIndexInAxis(parallelAxis, b->to().get());
        if (boundFrom > boundTo)
        {
            LOG_WARN(LOGGER << "Swapping bound attributes 'from' and 'to'");
            std::swap<int>(boundFrom, boundTo);
        }
        //
        //  Get bound position and perform some checks
        ////////////////////////////////
        //  Check if bound conditions are applicable in that place
        //  and save them in our internal structure.
        //
        for (bsIt bs = b->substance().begin(); bs < b->substance().end(); bs++)
        {
            bool isHorizontal = parallelAxis == StructureAnalyzer::HORIZONTAL;
            int s = structAnalyzer->getSubstanceIndex(bs->name());

            //  Here positions are positions of the points in the axes (not the area positions).
            int hFrom   = isHorizontal ? boundFrom   : boundAt;
            int hTo     = isHorizontal ? boundTo     : boundAt + 1;
            int vFrom   = isHorizontal ? boundAt     : boundFrom;
            int vTo     = isHorizontal ? boundAt + 1 : boundTo;
            for (int h = hFrom; h < hTo; h++)
            {
                for (int v = vFrom; v < vTo; v++)
                {
                    if (isHorizontal)
                    {
                        if (v != sizeV)
                            applyBoundCondition(h, v, s, TOP, &*b, &*bs);
                        else
                            applyBoundCondition(h, v - 1, s, BOTTOM, &*b, &*bs);
                    }
                    else
                    {
                        if (h != sizeH)
                            applyBoundCondition(h, v, s, LEFT, &*b, &*bs);
                        else
                            applyBoundCondition(h - 1, v, s, RIGHT, &*b, &*bs);
                    }
                }   //  for (int v = vFrom; v < vTo; v++)
            }       //  for (int h = hFrom; h < hTo; h++)
        }           //  for (bsIt bs = b->substance().begin(); bs < b->substance().end(); bs++)
        //
        //  Check if bound conditions are applicable in that place...
        ////////////////////////////////

    }   //  for (bIt b = model->bound().begin(); b < model->bound().end(); b++)
    LOG_INFO(LOGGER << "Applying all provided bound conditions... Done");
    //
    //  Get all bound specs from the model.
    /////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////
    //  Guess all missing bound conditions (or fail to do that)
    //
    LOG_INFO(LOGGER << "Trying to guess all remaining bound conditions...");
    for (int h = 0; h < sizeH; h++)
    {
        for (int v = 0; v < sizeV; v++)
        {
            for (int s = 0; s < sizeS; s++)
            {
                if (boundSubstances[h][v][s][TOP]->boundSubstance == 0)
                    applyBoundCondition(h, v, s, TOP, 0, 0);

                if (boundSubstances[h][v][s][RIGHT]->boundSubstance == 0)
                    applyBoundCondition(h, v, s, RIGHT, 0, 0);

                if (boundSubstances[h][v][s][BOTTOM]->boundSubstance == 0)
                    applyBoundCondition(h, v, s, BOTTOM, 0, 0);

                if (boundSubstances[h][v][s][LEFT]->boundSubstance == 0)
                    applyBoundCondition(h, v, s, LEFT, 0, 0);
            }
        }
    }
    LOG_INFO(LOGGER << "Trying to guess all remaining bound conditions... Done");
    //
    //  Guess all missing bound conditions (or fail to do that)
    /////////////////////////////////////////////////////////////
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_CFG_NS::BoundAnalyzer::~BoundAnalyzer()
{
    // Delete allocated bound specifications.
    for (std::vector< BIO_XML_NS::model::BoundSubstance* >::iterator bound = allocatedBoundConditions.begin();
            bound < allocatedBoundConditions.end(); bound++)
    {
        delete (*bound);
    }

    // Delete our big data structure...
    for (int h = 0; h < sizeH; h++)
    {
        for (int v = 0; v < sizeV; v++)
        {
            for (int s = 0; s < sizeS; s++)
            {
                for (int i = 0; i < 4; i++)
                {
                    delete boundSubstances[h][v][s][i];
                }
                delete [] boundSubstances[h][v][s];
            }
            delete [] boundSubstances[h][v];
        }
        delete [] boundSubstances[h];
    }
    delete [] boundSubstances;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_XML_NS::model::BoundSubstance* BIO_CFG_NS::BoundAnalyzer::getBoundForSubstance(int s, int h, int v, AreaSide side)
{
    return boundSubstances[h][v][s][side]->boundSubstance;
}


/* ************************************************************************** */
/* ************************************************************************** */
std::string* BIO_CFG_NS::BoundAnalyzer::getBoundName(int s, int h, int v, AreaSide side)
{
    return (boundSubstances[h][v][s][side]->bound == 0 ||
            !boundSubstances[h][v][s][side]->bound->name().present())
           ? 0
           : &boundSubstances[h][v][s][side]->bound->name().get();
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_CFG_NS::BoundAnalyzer::applyBoundCondition(
    int h,
    int v,
    int s,
    AreaSide side,
    BIO_XML_NS::model::Bound* bProvided,
    BIO_XML_NS::model::BoundSubstance* bsProvided
)
{
    //
    //  get diffusions in the adjacent areas.
    //
    int hPrev = (side == LEFT   ) ? h - 1 : h;
    int hNext = (side == RIGHT  ) ? h + 1 : h;
    int vPrev = (side == TOP    ) ? v - 1 : v;
    int vNext = (side == BOTTOM ) ? v + 1 : v;

    BIO_XML_NS::model::Symbol* diffPrev;
    BIO_XML_NS::model::Symbol* diffNext;
    if (side == TOP || side == BOTTOM) // isHorizontal
    {
        diffPrev = (vPrev == -1   ) ? 0 : structAnalyzer->getDiffusion(s, hPrev, vPrev);
        diffNext = (vNext == sizeV) ? 0 : structAnalyzer->getDiffusion(s, hNext, vNext);
    }
    else // isVertical
    {
        diffPrev = (hPrev == -1   ) ? 0 : structAnalyzer->getDiffusion(s, hPrev, vPrev);
        diffNext = (hNext == sizeH) ? 0 : structAnalyzer->getDiffusion(s, hNext, vNext);
    }
    //// FIXME: This solution (with *Null) is not correct.
    //          We must analze situations when there is no diffusion and when
    //          there is no substance in the area differently.
    //
    //  Assumption that no substance is the same as diffusion is 0 is not
    //  really correct. This is false in those cases, when substance has no
    //  diffusion but participates in a reaction. Example of such substance
    //  is an enzyme.
    //bool diffPrevIsZero = diffPrev == 0 || diffPrev->value() == 0.0;
    //bool diffNextIsZero = diffNext == 0 || diffNext->value() == 0.0;
    bool diffPrevIsNull = diffPrev == 0;
    bool diffNextIsNull = diffNext == 0;


    BIO_XML_NS::model::SubstanceName& substanceName = structAnalyzer->getSubstances()[s]->name();

    BoundSubstanceInfo bsPrev;
    BoundSubstanceInfo bsNext;
    if (bsProvided)
    {
        //
        //  Check the applicability of the condition
        //
        if (dynamic_cast<BIO_XML_NS::model::bound::Merge*>(bsProvided))
        {
            if (diffPrevIsNull || diffNextIsNull)
                throw Exception("In one of the sides of Merge bound is no substance or diffusion is 0.");

            bsPrev.bound            = bProvided;
            bsPrev.boundSubstance   = bsProvided;
            bsNext.bound            = bProvided;
            bsNext.boundSubstance   = bsProvided;
        }
        else if (dynamic_cast<BIO_XML_NS::model::bound::Constant*>(bsProvided) ||
                 dynamic_cast<BIO_XML_NS::model::bound::Wall*>(bsProvided))
        {
            if (diffPrevIsNull && diffNextIsNull)
                throw Exception("Both sides of 'Wall' or 'Constant' bound have no substrate of diffusions are 0.");

            if (!diffPrevIsNull && !diffNextIsNull)
                throw Exception("Both sides of 'Wall' or 'Constant' bound have non 0 diffusions so I think the model is inconsistent");

            if (diffPrevIsNull)
            {
                bsPrev.bound = 0;
                allocatedBoundConditions.push_back(
                    bsPrev.boundSubstance = new BIO_XML_NS::model::bound::Null(substanceName)
                );
                bsNext.bound = bProvided;
                bsNext.boundSubstance = bsProvided;
            }
            else // diffNextIsNull
            {
                bsPrev.bound = bProvided;
                bsPrev.boundSubstance = bsProvided;
                bsNext.bound = 0;
                allocatedBoundConditions.push_back(
                    bsNext.boundSubstance = new BIO_XML_NS::model::bound::Null(substanceName)
                );
            }
        }
        else if (dynamic_cast<BIO_XML_NS::model::bound::Null*>(bsProvided))
        {
            if (!diffPrevIsNull || !diffNextIsNull)
                throw Exception("One of the sides of 'Null' bound have non 0 diffusions so I think the model is inconsistent");

            bsPrev.bound            = bProvided;
            bsPrev.boundSubstance   = bsProvided;
            bsNext.bound            = bProvided;
            bsNext.boundSubstance   = bsProvided;
        }
        else
        {
            throw Exception("Unknown BoundSubstance type");
        }
    }   // if (bsProvided)
    else
    {
        // try to guess correct BS

        if (!diffPrevIsNull && !diffNextIsNull)
        {
            bsPrev.bound = bsNext.bound = 0;
            allocatedBoundConditions.push_back(
                bsPrev.boundSubstance = bsNext.boundSubstance = new BIO_XML_NS::model::bound::Merge(substanceName)
            );
        }
        else if (diffPrevIsNull && diffNextIsNull)
        {
            bsPrev.bound = bsNext.bound = 0;
            allocatedBoundConditions.push_back(
                bsPrev.boundSubstance = bsNext.boundSubstance = new BIO_XML_NS::model::bound::Null(substanceName)
            );
        }
        else
        {
            //  Is this always correct?
            //  Maybe restrict this to internal bounds only?
            if (diffPrevIsNull)
            {
                bsPrev.bound = bsNext.bound = 0;
                allocatedBoundConditions.push_back(
                    bsPrev.boundSubstance = new BIO_XML_NS::model::bound::Null(substanceName)
                );
                allocatedBoundConditions.push_back(
                    bsNext.boundSubstance = new BIO_XML_NS::model::bound::Wall(substanceName)
                );
            }
            else // diffNextIsNull
            {
                bsPrev.bound = bsNext.bound = 0;
                allocatedBoundConditions.push_back(
                    bsPrev.boundSubstance = new BIO_XML_NS::model::bound::Wall(substanceName)
                );
                allocatedBoundConditions.push_back(
                    bsNext.boundSubstance = new BIO_XML_NS::model::bound::Null(substanceName)
                );
            }
        }
    }


    //
    //  Save the bound conditions in the correct places.
    //
    if (side == TOP || side == BOTTOM) // isHorizontal
    {
        if (vPrev >= 0)
            *boundSubstances[hPrev][vPrev][s][BOTTOM] = bsPrev;

        if (vNext < sizeV)
            *boundSubstances[hNext][vNext][s][TOP] = bsNext;
    }
    else // isVertical
    {
        if (hPrev >= 0)
            *boundSubstances[hPrev][vPrev][s][RIGHT] = bsPrev;

        if (hNext < sizeH)
            *boundSubstances[hNext][vNext][s][LEFT] = bsNext;
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
