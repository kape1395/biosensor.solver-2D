#include "BoundAnalyzer.hxx"
#include "../Exception.hxx"
#include "../Logging.hxx"
#include "ReactionAnalyzer.hxx"
#include <vector>
#define LOGGER "libbiosensor::BoundAnalyzer: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_CFG_NS::BoundAnalyzer::BoundSubstanceInfo::BoundSubstanceInfo()
{
    boundSubstance = 0;
    derivedFromBound = 0;
    relatedReactions.clear();
}


BIO_CFG_NS::BoundAnalyzer::BoundSubstanceInfo&
BIO_CFG_NS::BoundAnalyzer::BoundSubstanceInfo::operator = (BoundSubstanceInfo& source)
{
    boundSubstance = source.boundSubstance;
    derivedFromBound = source.derivedFromBound;
    relatedReactions = source.relatedReactions;
    return *this;
}

/* ************************************************************************** */
/* ************************************************************************** */
BIO_CFG_NS::BoundAnalyzer::BoundInfo::BoundInfo()
{
    boundDefinition = 0;
    boundSubstances.clear();
}

BIO_CFG_NS::BoundAnalyzer::BoundInfo::~BoundInfo()
{
    boundDefinition = 0;
    boundSubstances.clear();
}

BIO_XML_MODEL_NS::Bound*
BIO_CFG_NS::BoundAnalyzer::BoundInfo::getBoundDefinition()
{
    return boundDefinition;
}

void
BIO_CFG_NS::BoundAnalyzer::BoundInfo::setBoundDefinition(BIO_XML_MODEL_NS::Bound* boundDef)
{
    this->boundDefinition = boundDef;
}

void
BIO_CFG_NS::BoundAnalyzer::BoundInfo::setSubstanceCount(int substCount)
{
    boundSubstances.clear();
    for (int i = 0; i < substCount; i++)
        boundSubstances.push_back(BoundSubstanceInfo());
}

BIO_CFG_NS::BoundAnalyzer::BoundSubstanceInfo&
BIO_CFG_NS::BoundAnalyzer::BoundInfo::operator[] (int substIndex)
{
    return boundSubstances[substIndex];
}

/* ************************************************************************** */
/* ************************************************************************** */
BIO_CFG_NS::BoundAnalyzer::BoundAnalyzer(
    StructureAnalyzer *structAnalyzer
)
{
    typedef BIO_XML_MODEL_NS::Model::bound_iterator bIt;
    typedef BIO_XML_MODEL_NS::Bound::substance_iterator bsIt;

    this->structAnalyzer = structAnalyzer;
    this->sizeH = structAnalyzer->getPointsH().size() - 1;
    this->sizeV = structAnalyzer->getPointsV().size() - 1;
    this->sizeS = structAnalyzer->getSubstances().size();

    bounds = new BoundInfo **[sizeH];
    for (int h = 0; h < sizeH; h++)
    {
        bounds[h] = new BoundInfo *[sizeV];
        for (int v = 0; v < sizeV; v++)
        {
            bounds[h][v] = new BoundInfo [4];    // 4 sides
            for (int i = 0; i < 4; i++)
            {
                bounds[h][v][i].setSubstanceCount(sizeS);
            }
        }
    }


    /////////////////////////////////////////////////////////////
    //  Get all bound specs from the model.
    //
    LOG_INFO(LOGGER << "Applying all provided bound conditions...");
    BIO_XML_MODEL_NS::Model *model = structAnalyzer->getConfig();
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
        bool isHorizontal = parallelAxis == StructureAnalyzer::HORIZONTAL;

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
                        applyBoundConditions(h, v, TOP, &*b);
                    if (v != 0)
                        applyBoundConditions(h, v - 1, BOTTOM, &*b);
                }
                else
                {
                    if (h != sizeH)
                        applyBoundConditions(h, v, LEFT, &*b);
                    if (h != 0)
                        applyBoundConditions(h - 1, v, RIGHT, &*b);
                }
            }   //  for (int v = vFrom; v < vTo; v++)
        }       //  for (int h = hFrom; h < hTo; h++)
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
                LOG_DEBUG(LOGGER << "Adding derived bound conditions for area h=" << h << " v=" << v << " and substabceIndex=" << s << "...");
                LOG_DEBUG(LOGGER << "Adding derived bound conditions side=" << TOP << " of this area.");
                if (bounds[h][v][TOP][s].boundSubstance == 0)
                    applyBoundCondition(h, v, s, TOP, 0, 0);

                LOG_DEBUG(LOGGER << "Adding derived bound conditions side=" << RIGHT << " of this area.");
                if (bounds[h][v][RIGHT][s].boundSubstance == 0)
                    applyBoundCondition(h, v, s, RIGHT, 0, 0);

                LOG_DEBUG(LOGGER << "Adding derived bound conditions side=" << BOTTOM << " of this area.");
                if (bounds[h][v][BOTTOM][s].boundSubstance == 0)
                    applyBoundCondition(h, v, s, BOTTOM, 0, 0);

                LOG_DEBUG(LOGGER << "Adding derived bound conditions side=" << LEFT << " of this area.");
                if (bounds[h][v][LEFT][s].boundSubstance == 0)
                    applyBoundCondition(h, v, s, LEFT, 0, 0);

                LOG_DEBUG(LOGGER << "Adding derived bound conditions for area h=" << h << " v=" << v << " and substabceIndex=" << s << "... done");
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
    for (std::vector< BIO_XML_MODEL_NS::BoundSubstance* >::iterator bound = allocatedBoundConditions.begin();
            bound < allocatedBoundConditions.end(); bound++)
    {
        delete (*bound);
    }

    // Delete our big data structure...
    for (int h = 0; h < sizeH; h++)
    {
        for (int v = 0; v < sizeV; v++)
        {
            delete [] bounds[h][v];
        }
        delete [] bounds[h];
    }
    delete [] bounds;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_XML_MODEL_NS::BoundSubstance* BIO_CFG_NS::BoundAnalyzer::getBoundForSubstance(int s, int h, int v, AreaSide side)
{
    return bounds[h][v][side][s].boundSubstance;
}


/* ************************************************************************** */
/* ************************************************************************** */
std::string* BIO_CFG_NS::BoundAnalyzer::getBoundName(int s, int h, int v, AreaSide side)
{
    return (bounds[h][v][side][s].derivedFromBound == 0 ||
            !bounds[h][v][side][s].derivedFromBound->name().present())
           ? 0
           : &bounds[h][v][side][s].derivedFromBound->name().get();
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_CFG_NS::BoundAnalyzer::applyBoundConditions(
    int h,
    int v,
    AreaSide side,
    BIO_XML_MODEL_NS::Bound* bound
)
{
    LOG_DEBUG(LOGGER
              << "Applying bound (name=" << (bound->name().present() ? bound->name().get() : std::string("n/a"))
              << ") to h=" << h << " v=" << v << " side=" << side
             );
    
    if (bounds[h][v][side].getBoundDefinition())
        throw Exception("Overlapping bound definitions are not supported.");

    bounds[h][v][side].setBoundDefinition(bound);

    typedef BIO_XML_MODEL_NS::Bound::substance_iterator bsIt;
    for (bsIt bs = bound->substance().begin(); bs < bound->substance().end(); bs++)
    {
        int s = structAnalyzer->getSubstanceIndex(bs->name());
        applyBoundCondition(h, v, s, side, bound, &*bs);
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_CFG_NS::BoundAnalyzer::applyBoundCondition(
    int h,
    int v,
    int s,
    AreaSide side,
    BIO_XML_MODEL_NS::Bound* bProvided,
    BIO_XML_MODEL_NS::BoundSubstance* bsProvided
)
{
    //
    //  get diffusions in the adjacent areas.
    //
    int hPrev = (side == LEFT   ) ? h - 1 : h;
    int hNext = (side == RIGHT  ) ? h + 1 : h;
    int vPrev = (side == TOP    ) ? v - 1 : v;
    int vNext = (side == BOTTOM ) ? v + 1 : v;

    BIO_XML_MODEL_NS::Symbol* diffPrev;
    BIO_XML_MODEL_NS::Symbol* diffNext;
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


    BIO_XML_MODEL_NS::SubstanceName& substanceName = structAnalyzer->getSubstances()[s]->name();

    BoundSubstanceInfo bsPrev;
    BoundSubstanceInfo bsNext;
    if (bsProvided)
    {
        //
        //  Check the applicability of the condition
        //
        if (dynamic_cast<BIO_XML_MODEL_NS::bound::Merge*>(bsProvided))
        {
            if (diffPrevIsNull || diffNextIsNull)
                throw Exception("In one of the sides of Merge bound is no substance or diffusion is 0.");

            bsPrev.derivedFromBound = bProvided;
            bsPrev.boundSubstance   = bsProvided;
            bsNext.derivedFromBound = bProvided;
            bsNext.boundSubstance   = bsProvided;
        }
        else if (dynamic_cast<BIO_XML_MODEL_NS::bound::Constant*>(bsProvided) ||
                 dynamic_cast<BIO_XML_MODEL_NS::bound::Wall*>(bsProvided))
        {
            if (diffPrevIsNull && diffNextIsNull)
                throw Exception("Both sides of 'Wall' or 'Constant' bound have no substrate of diffusions are 0.");

            if (!diffPrevIsNull && !diffNextIsNull)
                throw Exception("Both sides of 'Wall' or 'Constant' bound have non 0 diffusions so I think the model is inconsistent");

            if (diffPrevIsNull)
            {
                bsPrev.derivedFromBound = 0;
                allocatedBoundConditions.push_back(
                    bsPrev.boundSubstance = new BIO_XML_MODEL_NS::bound::Null(substanceName)
                );
                bsNext.derivedFromBound = bProvided;
                bsNext.boundSubstance = bsProvided;
            }
            else // diffNextIsNull
            {
                bsPrev.derivedFromBound = bProvided;
                bsPrev.boundSubstance = bsProvided;
                bsNext.derivedFromBound = 0;
                allocatedBoundConditions.push_back(
                    bsNext.boundSubstance = new BIO_XML_MODEL_NS::bound::Null(substanceName)
                );
            }
        }
        else if (dynamic_cast<BIO_XML_MODEL_NS::bound::Null*>(bsProvided))
        {
            if (!diffPrevIsNull || !diffNextIsNull)
                throw Exception("One of the sides of 'Null' bound have non 0 diffusions so I think the model is inconsistent");

            bsPrev.derivedFromBound = bProvided;
            bsPrev.boundSubstance   = bsProvided;
            bsNext.derivedFromBound = bProvided;
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
            bsPrev.derivedFromBound = bsNext.derivedFromBound = 0;
            allocatedBoundConditions.push_back(
                bsPrev.boundSubstance = bsNext.boundSubstance = new BIO_XML_MODEL_NS::bound::Merge(substanceName)
            );
        }
        else if (diffPrevIsNull && diffNextIsNull)
        {
            bsPrev.derivedFromBound = bsNext.derivedFromBound = 0;
            allocatedBoundConditions.push_back(
                bsPrev.boundSubstance = bsNext.boundSubstance = new BIO_XML_MODEL_NS::bound::Null(substanceName)
            );
        }
        else
        {
            //  Is this always correct?
            //  Maybe restrict this to internal bounds only?
            if (diffPrevIsNull)
            {
                bsPrev.derivedFromBound = bsNext.derivedFromBound = 0;
                allocatedBoundConditions.push_back(
                    bsPrev.boundSubstance = new BIO_XML_MODEL_NS::bound::Null(substanceName)
                );
                allocatedBoundConditions.push_back(
                    bsNext.boundSubstance = new BIO_XML_MODEL_NS::bound::Wall(substanceName)
                );
            }
            else // diffNextIsNull
            {
                bsPrev.derivedFromBound = bsNext.derivedFromBound = 0;
                allocatedBoundConditions.push_back(
                    bsPrev.boundSubstance = new BIO_XML_MODEL_NS::bound::Wall(substanceName)
                );
                allocatedBoundConditions.push_back(
                    bsNext.boundSubstance = new BIO_XML_MODEL_NS::bound::Null(substanceName)
                );
            }
        }
    }


    collectRelatedReactions(bounds[h][v][side], bsPrev);
    collectRelatedReactions(bounds[h][v][side], bsNext);


    //
    //  Save the bound conditions in the correct places.
    //
    if (side == TOP || side == BOTTOM) // isHorizontal
    {
        if (vPrev >= 0)
            bounds[hPrev][vPrev][BOTTOM][s] = bsPrev;

        if (vNext < sizeV)
            bounds[hNext][vNext][TOP][s] = bsNext;
    }
    else // isVertical
    {
        if (hPrev >= 0)
            bounds[hPrev][vPrev][RIGHT][s] = bsPrev;

        if (hNext < sizeH)
            bounds[hNext][vNext][LEFT][s] = bsNext;
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_CFG_NS::BoundAnalyzer::collectRelatedReactions(BoundInfo& bi, BoundSubstanceInfo& bsi)
{
    if (!bi.getBoundDefinition())
        return;

    BIO_XML_MODEL_NS::Bound::reaction_iterator rit;
    BIO_XML_MODEL_NS::Bound::reaction_sequence rs = bi.getBoundDefinition()->reaction();
    for (rit = rs.begin(); rit != rs.end(); rit++)
    {
        BIO_XML_MODEL_NS::Reaction* r = this->structAnalyzer->getReaction(rit->name());

        ReactionAnalyzer* ra = ReactionAnalyzer::newAnalyzer(r);
        if (ra->isSubstrate(bsi.boundSubstance->name()) ||
                ra->isProduct(bsi.boundSubstance->name()))
        {
            bsi.relatedReactions.push_back(r);
            LOG_DEBUG(LOGGER << "Reaction " << r->name() << " is related to the substance " << bsi.boundSubstance->name());
        }
        delete ra;
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
