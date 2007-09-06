#include "dm_Abstract.hh"


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Konstruktorius.
 */
dm::Model::Model(
    cfg::Config*    configuration,
    PointFactory*   pointFactory,
    ModelFactory*   modelFactory
)
{
    this->configuration = configuration;
    this->modelFactory = modelFactory;
    this->partsH  = configuration->getDimensionXParts().size();
    this->partsV  = configuration->getDimensionYParts().size();

    ///////////////////////////////////////
    //  Susukuriam horizontalias dimensijas
    this->dimH = new Dimension*[partsH];
    {
        std::list<cfg::DimensionPart*>::iterator iter = configuration->getDimensionXParts().begin();
        std::list<cfg::DimensionPart*>::iterator end  = configuration->getDimensionXParts().end();
        double startPosition = 0.0;
        for (int i = 0; iter != end; i++, iter++)
        {
            dimH[i] = createDimensionByConfig(*iter, HORIZONTAL, startPosition);
            startPosition += (*iter)->getLength();
        }
    }

    ///////////////////////////////////////
    //  Susukuriam vertikalias dimensijas
    this->dimV = new Dimension*[partsV];
    {
        std::list<cfg::DimensionPart*>::iterator iter = configuration->getDimensionYParts().begin();
        std::list<cfg::DimensionPart*>::iterator end  = configuration->getDimensionYParts().end();
        double startPosition = 0.0;
        for (int i = 0; iter != end; i++, iter++)
        {
            dimV[i] = createDimensionByConfig(*iter, VERTICAL, startPosition);
            startPosition += (*iter)->getLength();
        }
    }


    ///////////////////////////////////////
    //  Sukuriam striciu vidus.
    area = new Area**[this->partsH];
    for (int i = 0; i < partsH; i++)
    {
        area[i] = new Area*[partsV];
        for (int j = 0; j < partsV; j++)
        {
            cfg::Area* config = 0;
            std::list<cfg::Area*>::iterator it = configuration->getAreas().begin();
            for ( ; it != configuration->getAreas().end(); it++)
                if ((*it)->getPositionX() == i && (*it)->getPositionY() == j)
                {
                    config = *it;
                    break;
                }
            area[i][j] = modelFactory->newArea(
                             config,
                             pointFactory,
                             this->dimH[i],
                             this->dimV[j]
                         );
        }
    }

    ///////////////////////////////////////
    //  Sukuriam horizontalius krastus.
    boundH = new Bound**[partsH];
    for (int i = 0; i < partsH; i++)
    {
        boundH[i] = new Bound*[partsV + 1];
        for (int j = 0; j <= partsV; j++)
        {
            cfg::Bound* config = 0;
            std::list<cfg::Bound*>::iterator it = configuration->getBounds().begin();
            for ( ; it != configuration->getBounds().end(); it++)
                if (
                    ((*it)->getPositionX() == i &&
                     (*it)->getPositionY() == j &&
                     (*it)->getSide() == cfg::Bound::TOP) ||
                    ((*it)->getPositionX() == i &&
                     (*it)->getPositionY() == j - 1 &&
                     (*it)->getSide() == cfg::Bound::BOTTOM))
                {
                    config = *it;
                    break;
                }
            boundH[i][j] = modelFactory->newBound(
                               config,
                               pointFactory,
                               this->dimH[i],
                               j == 0      ? 0 : area[i][j - 1],
                               j == partsV ? 0 : area[i][j]
                           );
        }
    }

    ///////////////////////////////////////
    //  Sukuriam vertikalius krastus.
    boundV = new Bound**[partsH + 1];
    for (int i = 0; i <= partsH; i++)
    {
        boundV[i] = new Bound*[partsV];
        for (int j = 0; j < partsV; j++)
        {
            cfg::Bound* config = 0;
            std::list<cfg::Bound*>::iterator it = configuration->getBounds().begin();
            for ( ; it != configuration->getBounds().end(); it++)
                if (
                    ((*it)->getPositionX() == i &&
                     (*it)->getPositionY() == j &&
                     (*it)->getSide() == cfg::Bound::LEFT) ||
                    ((*it)->getPositionX() == i - 1 &&
                     (*it)->getPositionY() == j &&
                     (*it)->getSide() == cfg::Bound::RIGHT))
                {
                    config = *it;
                    break;
                }
            boundV[i][j] = modelFactory->newBound(
                               config,
                               pointFactory,
                               this->dimV[j],
                               i == 0      ? 0 : area[i - 1][j],
                               i == partsH ? 0 : area[i][j]
                           );
        }
    }

    ///////////////////////////////////////
    //  Sukuriam kampus.
    corner = new Corner**[partsH + 1];
    for (int i = 0; i <= partsH; i++)
    {
        corner[i] = new Corner*[partsV + 1];
        for (int j = 0; j <= partsV; j++)
            corner[i][j] = modelFactory->newCorner(
                               pointFactory,
                               j == 0      ? 0 : boundV[i][j - 1],
                               i == partsH ? 0 : boundH[i][j],
                               j == partsV ? 0 : boundV[i][j],
                               i == 0      ? 0 : boundH[i - 1][j]
                           );
    }

}



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Konstruktorius.
 */
dm::Model::~Model()
{

    for (int i = 0; i <= partsH; i++)
    {
        for (int j = 0; j <= partsV; j++)
            delete corner[i][j];
        delete[] corner[i];
    }
    delete[] corner;


    for (int i = 0; i < partsH; i++)
    {
        for (int j = 0; j <= partsV; j++)
            delete boundH[i][j];
        delete[] boundH[i];
    }
    delete[] boundH;


    for (int i = 0; i <= partsH; i++)
    {
        for (int j = 0; j < partsV; j++)
            delete boundV[i][j];
        delete[] boundV[i];
    }
    delete[] boundV;


    for (int i = 0; i < partsH; i++)
    {
        for (int j = 0; j < partsV; j++)
            delete area[i][j];
        delete[] area[i];
    }
    delete[] area;


    for (int i = 0; i < partsH; i++)
        delete dimH[i];
    delete[] dimH;


    for (int i = 0; i < partsV; i++)
        delete dimV[i];
    delete[] dimV;
}


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Pagal konfiguracija sukuria dimensijos duomenu struktura.
 */
dm::Dimension* dm::Model::createDimensionByConfig(
    cfg::DimensionPart* config,
    Direction direction,
    double startPosition
)
{
    Dimension* dim = 0;
    if (config->getType() == "DEFAULT")
    {
        dim = new ConstantDimension(
                  direction,
                  startPosition,
                  config->getLength(),
                  2
              );
    }
    else if (config->getType() == "CONSTANT")
    {
        dim = new ConstantDimension(
                  direction,
                  startPosition,
                  config->getLength(),
                  dynamic_cast<cfg::ConstantDimensionPart*>(config)->getStepCount()
              );
    }
    else if (config->getType() == "BILINEAR")
    {
        // FIXME: Implement biliner dimension.
        std::cerr << "ERROR: Bilinera dimension not implemented yet.\n";
    }
    else
    {
        std::cerr << "ERROR: Unknown domensionpart type " << config->getType() << '\n';
    }

    return dim;
}



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Grazina substane`o indeksa duomenu modelyje.
 */
int dm::Model::getSubstanceIndex(cfg::Substance* substance)
{
    std::list<cfg::Substance*>::iterator itSubs = configuration->getSubstances().begin();
    std::list<cfg::Substance*>::iterator itSEnd = configuration->getSubstances().end();
    for (int i = 0; itSubs != itSEnd; itSubs++, i++)
    {
        if (substance == (*itSubs))
            return i;
    }
    std::cerr << "ERROR: Substance not found, substance=" << substance << '\n';
    return -1;  // FIXME: cia exception.
}




/* ************************************************************************** */
/* ************************************************************************** */


/**
 *  Area konstruktorius.
 */
dm::Area::Area(
    cfg::Area*  configuration,
    Dimension*  dimX,
    Dimension*  dimY
)
{
    this->configuration = configuration;
    this->dimX = dimX;
    this->dimY = dimY;
}



/**
 *  Bound konstruktorius.
 */
dm::Bound::Bound(
    cfg::Bound* configuration,
    Dimension*  dim,
    Area*       prevArea,
    Area*       nextArea
)
{
    this->configuration = configuration;
    this->dim      = dim;
    this->prevArea = prevArea;
    this->nextArea = nextArea;
    switch (dim->getDirection())
    {
    case (HORIZONTAL):
                    if (nextArea != 0)
                        nextArea->boundTop = this;

        if (prevArea != 0)
            prevArea->boundBottom = this;

        break;
    case (VERTICAL):
                    if (nextArea != 0)
                        nextArea->boundLeft = this;

        if (prevArea != 0)
            prevArea->boundRight = this;

        break;
    }
}



/**
 *  Corner konstruktorius.
 */
dm::Corner::Corner(
    Bound* top,
    Bound* right,
    Bound* bottom,
    Bound* left
)
{
    this->top    = top;
    this->right  = right;
    this->bottom = bottom;
    this->left   = left;

    if (top != 0)
        top->nextCorner = this;

    if (right != 0)
        right->prevCorner = this;

    if (bottom != 0)
        bottom->prevCorner = this;

    if (left != 0)
        left->nextCorner = this;

}




/* ************************************************************************** */
/* **********   ConstantDimenion   ****************************************** */
/* ************************************************************************** */



/**
 *  Konstruktorius.
 *  @param direction Tipas (H/V).
 *  @param start     Dimensijos pradzia.
 *  @param length    Dimensijos ilgis.
 *  @param steps     Kiek zinsniu srityje (tasku bus 1 daugiau).
 */
dm::ConstantDimension::ConstantDimension(
    Direction   direction,
    double      start,
    double      length,
    int         steps
) : dm::Dimension(direction, start, length)
{
    this->steps = steps;

    positions = new double[steps + 1];
    intervals = new double[steps];

    for (int i = 0; i < steps + 1; i++)
    {
        positions[i] = start + (length / steps * i);
    }

    for (int i = 0; i < steps; i++)
    {
        intervals[i] = positions[i + 1] - positions[i];
    }
}



/**
 *  Destruktorius.
 */
dm::ConstantDimension::~ConstantDimension()
{
    delete[] positions;
    delete[] intervals;
}


/* ************************************************************************** */
/* ************************************************************************** */
