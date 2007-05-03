#define DM_Abstract_CC
#include "Abstract.hh"

namespace dm
{



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Konstruktorius.
 */
Model::Model(
    PointFactory*   pointFactory,
    ModelFactory*   modelFactory,
    int             partsH,
    int             partsV,
    DimensionList&  dimH,
    DimensionList&  dimV
)
{
    this->modelFactory = modelFactory;
    this->partsH  = partsH;
    this->partsV  = partsV;

    this->dimH = new Dimension*[dimH.size()];
    this->dimV = new Dimension*[dimV.size()];
    {
        int i;

        i = 0;
        for (DimensionList::iterator it = dimH.begin(); it != dimH.end(); it++, i++)
            this->dimH[i] = *it;

        i = 0;
        for (DimensionList::iterator it = dimV.begin(); it != dimV.end(); it++, i++)
            this->dimV[i] = *it;
    }


    area = new Area**[this->partsH];
    for (int i = 0; i < partsH; i++)
    {
        area[i] = new Area*[partsV];
        for (int j = 0; j < partsV; j++)
            area[i][j] = modelFactory->newArea(pointFactory, this->dimH[i], this->dimV[j]);
    }


    boundH = new Bound**[partsH];
    for (int i = 0; i < partsH; i++)
    {
        boundH[i] = new Bound*[partsV + 1];
        for (int j = 0; j <= partsV; j++)
            boundH[i][j] = modelFactory->newBound(
                               pointFactory, 
                               this->dimH[i],
                               j == 0      ? 0 : area[i][j - 1],
                               j == partsV ? 0 : area[i][j]
                           );
    }


    boundV = new Bound**[partsH + 1];
    for (int i = 0; i <= partsH; i++)
    {
        boundV[i] = new Bound*[partsV];
        for (int j = 0; j < partsV; j++)
            boundV[i][j] = modelFactory->newBound(
                               pointFactory,
                               this->dimV[j],
                               i == 0      ? 0 : area[i - 1][j],
                               i == partsH ? 0 : area[i][j]
                           );
    }


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
Model::~Model()
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

    delete[] dimH;
    delete[] dimV;
}



/* ************************************************************************** */
/* ************************************************************************** */
}   // namespace dm
