#include "BoundAnalyzer.hxx"
#include "../Exception.hxx"


/* ************************************************************************** */
/* ************************************************************************** */
BIO_CFG_NS::BoundAnalyzer::BoundAnalyzer(StructureAnalyzer *structAnalyzer)
{
    typedef BIO_XML_NS::model::Model::bound_iterator bIt;
    typedef BIO_XML_NS::model::Bound::substance_iterator bsIt;

    this->structAnalyzer = structAnalyzer;
    this->sizeH = structAnalyzer->getPointsH().size() - 1;
    this->sizeV = structAnalyzer->getPointsV().size() - 1;
    this->sizeS = structAnalyzer->getSubstances().size();

    bounds = new BIO_XML_NS::model::BoundSubstance* ***[sizeH];
    for (int h = 0; h < sizeH; h++)
    {
        bounds[h] = new BIO_XML_NS::model::BoundSubstance* **[sizeV];
        for (int v = 0; v < sizeV; v++)
        {
            bounds[h][v] = new BIO_XML_NS::model::BoundSubstance* *[sizeS];
            for (int s = 0; s < sizeS; s++)
            {
                bounds[h][v][s] = new BIO_XML_NS::model::BoundSubstance* [4];    // 4 sides
                for (int i = 0; i < sizeS; i++)
                {
                    bounds[h][v][s][i] = 0;
                }
            }
        }
    }


    /////////////////////////////////////////////////////////////
    //  Get all bound specs from the model.
    //
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
            LOG4CXX_WARN(log, "Swapping bound attributes 'from' and 'to'");
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
            int hTo     = isHorizontal ? boundTo     : boundAt;
            int vFrom   = isHorizontal ? boundAt     : boundFrom;
            int vTo     = isHorizontal ? boundAt     : boundTo;
            for (int h = hFrom; h < hTo; h++)
            {
                for (int v = vFrom; v < vTo; v++)
                {
                    if (isHorizontal)
                    {
                        if (v != sizeV - 1)
                            applyBoundCondition(h, v, s, TOP, &*bs);
                        else
                            applyBoundCondition(h - 1, v, s, BOTTOM, &*bs);
                    }
                    else
                    {
                        if (h != sizeH - 1)
                            applyBoundCondition(h, v, s, LEFT, &*bs);
                        else
                            applyBoundCondition(h, v - 1, s, RIGHT, &*bs);
                    }
// ---> start
                    /*
                                        //
                                        //  get diffusions in the adjacent areas.
                                        //
                                        BIO_XML_NS::model::Symbol* diffPrev;
                                        BIO_XML_NS::model::Symbol* diffNext;
                                        if (isHorizontal)
                                        {
                                            diffPrev = (v == 0        ) ? 0 : structAnalyzer->getDiffusion(s, h, v - 1);
                                            diffNext = (v == sizeV - 1) ? 0 : structAnalyzer->getDiffusion(s, h, v    );
                                        }
                                        else    // isVertical
                                        {
                                            diffPrev = (h == 0        ) ? 0 : structAnalyzer->getDiffusion(s, h - 1, v);
                                            diffNext = (h == sizeH - 1) ? 0 : structAnalyzer->getDiffusion(s, h    , v);
                                        }
                                        bool diffPrevIsZero = diffPrev == 0 || diffPrev->value() == 0.0;
                                        bool diffNextIsZero = diffNext == 0 || diffNext->value() == 0.0;

                                        //
                                        //  Check the applicability of the condition
                                        //
                                        BIO_XML_NS::model::BoundSubstance* bsPrev;
                                        BIO_XML_NS::model::BoundSubstance* bsNext;
                                        if (dynamic_cast<BIO_XML_NS::model::bound::Merge*>(&*bs))
                                        {
                                            if (diffPrevIsZero || diffNextIsZero)
                                                throw Exception("In one of the sides of Merge bound is no substance or diffusion is 0.");

                                            bsPrev = &*bs;
                                            bsNext = &*bs;
                                        }
                                        else if (dynamic_cast<BIO_XML_NS::model::bound::Constant*>(&*bs) ||
                                                 dynamic_cast<BIO_XML_NS::model::bound::Wall*>(&*bs))
                                        {
                                            if (diffPrevIsZero && diffNextIsZero)
                                                throw Exception("Both sides of 'Wall' or 'Constant' bound have no substrate of diffusions are 0.");

                                            if (!diffPrevIsZero && !diffNextIsZero)
                                                throw Exception("Both sides of 'Wall' or 'Constant' bound have non 0 diffusions so I think the model is inconsistent");

                                            if (diffPrevIsZero)
                                            {
                                                bsPrev = 0;
                                                bsNext = &*bs;
                                            }
                                            else // diffNextIsZero
                                            {
                                                bsPrev = &*bs;
                                                bsNext = 0;
                                            }
                                        }
                                        else if (dynamic_cast<BIO_XML_NS::model::bound::Null*>(&*bs))
                                        {
                                            if (!diffPrevIsZero || !diffNextIsZero)
                                                throw Exception("One of the sides of 'Null' bound have non 0 diffusions so I think the model is inconsistent");

                                            bsPrev = &*bs;
                                            bsNext = &*bs;
                                        }
                                        else
                                        {
                                            throw Exception("Unknown BoundSubstance type");
                                        }

                                        //
                                        //  Save the bound conditions in the correct places.
                                        //
                                        if (isHorizontal)
                                        {
                                            if (v != 0)
                                                bounds[h][v-1][s][BOTTOM] = createNullBoundSubstanceIfNull(bsPrev, bs->name());

                                            if (v != sizeV - 1)
                                                bounds[h][v][s][TOP] = createNullBoundSubstanceIfNull(bsNext, bs->name());
                                        }
                                        else    // isVertical
                                        {
                                            if (h != 0)
                                                bounds[h - 1][v][s][RIGHT] = createNullBoundSubstanceIfNull(bsPrev, bs->name());

                                            if (h != sizeH - 1)
                                                bounds[h][v][s][LEFT] = createNullBoundSubstanceIfNull(bsNext, bs->name());
                                        }
                    */
// ---> end
                }   //  for (int v = vFrom; v < vTo; v++)
            }       //  for (int h = hFrom; h < hTo; h++)
        }           //  for (bsIt bs = b->substance().begin(); bs < b->substance().end(); bs++)
        //
        //  Check if bound conditions are applicable in that place...
        ////////////////////////////////

    }   //  for (bIt b = model->bound().begin(); b < model->bound().end(); b++)
    //
    //  Get all bound specs from the model.
    /////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////
    //  Guess all missing bound conditions (or fail to do that)
    //
    for (int h = 0; h < sizeH; h++)
    {
        for (int v = 0; v < sizeV; v++)
        {
            for (int s = 0; s < sizeS; s++)
            {
                if (bounds[h][v][s][TOP] == 0)
                    applyBoundCondition(h, v, s, TOP, 0);

                if (bounds[h][v][s][RIGHT] == 0)
                    applyBoundCondition(h, v, s, RIGHT, 0);

                if (bounds[h][v][s][BOTTOM] == 0)
                    applyBoundCondition(h, v, s, BOTTOM, 0);

                if (bounds[h][v][s][LEFT] == 0)
                    applyBoundCondition(h, v, s, LEFT, 0);
            }
        }
    }

    //
    //  Guess all missing bound conditions (or fail to do that)
    /////////////////////////////////////////////////////////////
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_CFG_NS::BoundAnalyzer::~BoundAnalyzer()
{
    // Delete allocated bound specifications.
    for (std::vector< BIO_XML_NS::model::BoundSubstance* >::iterator bound = allocatedBounds.begin();
            bound < allocatedBounds.end(); bound++)
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
                delete [] bounds[h][v][s];
            }
            delete [] bounds[h][v];
        }
        delete [] bounds[h];
    }
    delete [] bounds;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_XML_NS::model::BoundSubstance* BIO_CFG_NS::BoundAnalyzer::getBound(int s, int h, int v, AreaSide side)
{
    return bounds[h][v][s][side];
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_CFG_NS::BoundAnalyzer::applyBoundCondition(
    int h,
    int v,
    int s,
    AreaSide side,
    BIO_XML_NS::model::BoundSubstance* bsProvided
)
{
    //
    //  get diffusions in the adjacent areas.
    //
    BIO_XML_NS::model::Symbol* diffPrev;
    BIO_XML_NS::model::Symbol* diffNext;
    if (side == TOP || side == BOTTOM) // isHorizontal
    {
        diffPrev = (v == 0        ) ? 0 : structAnalyzer->getDiffusion(s, h, v - 1);
        diffNext = (v == sizeV - 1) ? 0 : structAnalyzer->getDiffusion(s, h, v    );
    }
    else // isVertical
    {
        diffPrev = (h == 0        ) ? 0 : structAnalyzer->getDiffusion(s, h - 1, v);
        diffNext = (h == sizeH - 1) ? 0 : structAnalyzer->getDiffusion(s, h    , v);
    }
    bool diffPrevIsZero = diffPrev == 0 || diffPrev->value() == 0.0;
    bool diffNextIsZero = diffNext == 0 || diffNext->value() == 0.0;


    
    BIO_XML_NS::model::BoundSubstance* bsPrev;
    BIO_XML_NS::model::BoundSubstance* bsNext;
    if (bsProvided)
    {
        //
        //  Check the applicability of the condition
        //
        if (dynamic_cast<BIO_XML_NS::model::bound::Merge*>(bsProvided))
        {
            if (diffPrevIsZero || diffNextIsZero)
                throw Exception("In one of the sides of Merge bound is no substance or diffusion is 0.");

            bsPrev = bsProvided;
            bsNext = bsProvided;
        }
        else if (dynamic_cast<BIO_XML_NS::model::bound::Constant*>(bsProvided) ||
                 dynamic_cast<BIO_XML_NS::model::bound::Wall*>(bsProvided))
        {
            if (diffPrevIsZero && diffNextIsZero)
                throw Exception("Both sides of 'Wall' or 'Constant' bound have no substrate of diffusions are 0.");

            if (!diffPrevIsZero && !diffNextIsZero)
                throw Exception("Both sides of 'Wall' or 'Constant' bound have non 0 diffusions so I think the model is inconsistent");

            if (diffPrevIsZero)
            {
                bsPrev = 0;
                bsNext = bsProvided;
            }
            else // diffNextIsZero
            {
                bsPrev = bsProvided;
                bsNext = 0;
            }
        }
        else if (dynamic_cast<BIO_XML_NS::model::bound::Null*>(bsProvided))
        {
            if (!diffPrevIsZero || !diffNextIsZero)
                throw Exception("One of the sides of 'Null' bound have non 0 diffusions so I think the model is inconsistent");

            bsPrev = bsProvided;
            bsNext = bsProvided;
        }
        else
        {
            throw Exception("Unknown BoundSubstance type");
        }
    }   // if (bsProvided)
    else
    {   // try to guess correct BS
        BIO_XML_NS::model::SubstanceName* substanceName = structAnalyzer->getSubstances(s)->name();

        if (!diffPrevIsZero && !diffNextIsZero)
        {
            diffPrev = diffNext = new BIO_XML_NS::model::bound::Merge(substanceName);
            allocatedBoundConditions.push_back(diffPrev);
        }
        else if (diffPrevIsZero && diffNextIsZero)
        {
            diffPrev = diffNext = new BIO_XML_NS::model::bound::Null(substanceName);
            allocatedBoundConditions.push_back(diffPrev);
        }
        else
        {
            //  Is this always correct?
            //  Maybe restrict this to internal bounds only?
            if (diffPrevIsZero)
            {
                allocatedBoundConditions.push_back(diffPrev = new BIO_XML_NS::model::bound::Null(substanceName));
                allocatedBoundConditions.push_back(diffNext = new BIO_XML_NS::model::bound::Wall(substanceName));
            }
            else // diffNextIsZero
            {
                allocatedBoundConditions.push_back(diffPrev = new BIO_XML_NS::model::bound::Wall(substanceName));
                allocatedBoundConditions.push_back(diffNext = new BIO_XML_NS::model::bound::Null(substanceName));
            }
        }
    }

    
    //
    //  Save the bound conditions in the correct places.
    //
    if (side == TOP || side == BOTTOM) // isHorizontal
    {
        if (v != 0)
            bounds[h][v-1][s][BOTTOM] = createNullBoundSubstanceIfNull(bsPrev, bsProvided->name());

        if (v != sizeV - 1)
            bounds[h][v][s][TOP] = createNullBoundSubstanceIfNull(bsNext, bsProvided->name());
    }
    else // isVertical
    {
        if (h != 0)
            bounds[h - 1][v][s][RIGHT] = createNullBoundSubstanceIfNull(bsPrev, bsProvided->name());

        if (h != sizeH - 1)
            bounds[h][v][s][LEFT] = createNullBoundSubstanceIfNull(bsNext, bsProvided->name());
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_XML_NS::model::BoundSubstance* BIO_CFG_NS::BoundAnalyzer::createNullBoundSubstanceIfNull(
    BIO_XML_NS::model::BoundSubstance* bs,
    BIO_XML_NS::model::SubstanceName& sn
)
{
    if (bs != 0)
        return bs;

    bs = new BIO_XML_NS::model::bound::Null(sn);
    allocatedBounds.push_back(bs);

    return bs;
}


/* ************************************************************************** */
/* ************************************************************************** */
