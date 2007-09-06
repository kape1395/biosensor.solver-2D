#include "ConfigAnalyzer.hh"
#include <cstdio>
#include "Exceptions.hh"
#include "xsd/Model.hh"
#include "xsd/ModelMediumReaction.hh"
#include "xsd/ModelBoundCondition.hh"


/* ************************************************************************** */
/* ************************************************************************** */



/**
 *
 */
cfg::ConfigAnalyzer2D::ConfigAnalyzer2D(::xsd::model::Model* model)
{
    logger = ::log4cxx::Logger::getLogger("bio.cfg.ConfigAnalyzer2D");
    this->model = model;
    areaMatrix  = 0;
    areaMatrixX = 0;
    areaMatrixY = 0;
    LOG4CXX_DEBUG(logger, "Constructor");
}



/**
 *
 */
cfg::ConfigAnalyzer2D::~ConfigAnalyzer2D()
{
    LOG4CXX_DEBUG(logger, "Destructor");
    if (areaMatrix != 0)
    {
        for (int i = 0; i < areaMatrixX; i++)
            delete[] areaMatrix[i];
        delete[] areaMatrix;
    }
}



/**
 *  \throws Exception
 */
void cfg::ConfigAnalyzer2D::analyze()
{
    LOG4CXX_DEBUG(logger, "analyze()");


    //  Patikrinam, ar sita klase palaiko nurodyta koordinaciu sistema.
    if (model->coordinateSystem() != xsd::model::CoordinateSystem::Cartesian &&
        model->coordinateSystem() != xsd::model::CoordinateSystem::Cylindrical)
        throw Exception(model->coordinateSystem() + " coordinate system" +
                " is not 2D, and therefore not suported by this class"
                );


    //  Ar asiu skaicius toks koks reikia?
    if (model->axis().size() != 2)
    {
        char buf[256];
        sprintf(buf, "Model has %i axes, %s coordinate system supports exact 2 axes",
            model->axis().size(),
            model->coordinateSystem().c_str()
            );
        throw new Exception(buf);
        //throw Exception("Model has " + atoi(model->axis().size()) + " axes," +
        //        model->coordinateSystem() + " coordinate system supports exact 2 axes"
        //        );
    }


    xsd::model::Axis* axisX = &model->axis()[0];
    xsd::model::Axis* axisY = &model->axis()[1];


    //  Susikuriam matrica, i kurios "langeliams" priskirinesime medziagas.
    areaMatrixX = axisX->point().size() - 1;
    areaMatrixY = axisY->point().size() - 1;
    areaMatrix = new xsd::model::Medium**[areaMatrixX];
    for (int x = 0; x < areaMatrixX; x++)
    {
        areaMatrix[x] = new xsd::model::Medium*[areaMatrixY];
        for (int y = 0; y < areaMatrixY; y++)
            areaMatrix[x][y] = 0;
    }


    //  Einam per visas medziagas, priskirsime jas matricos langeliams.
    for (xsd::model::Model::medium_iterator iMedium = model->medium().begin();
        iMedium != model->medium().end(); iMedium++)
    {
        //  Einam per visas sritis, kurios apibreztos tai medziagai.
        for (xsd::model::Medium::area_iterator iArea = iMedium->area().begin();
            iArea != iMedium->area().end(); iArea++)
        {
            if (!iArea->left().present() || !iArea->right ().present() ||
                !iArea->top ().present() || !iArea->bottom().present())
                throw Exception("For 2D model in area must be specified attributes: "
                        " left, right, top, bottom. Now some of them are empty"
                        );

            //  Priskiriame reikalingom celem sia medziaga.
            int areaL = boundIndexInAxis(*axisX, iArea->left    ().get());
            int areaR = boundIndexInAxis(*axisX, iArea->right   ().get());
            int areaT = boundIndexInAxis(*axisY, iArea->top     ().get());
            int areaB = boundIndexInAxis(*axisY, iArea->bottom  ().get());
            for (int x = areaL; x < areaR; x++)
            {
                for (int y = areaT; y < areaB; y++)
                {
                    if (areaMatrix[x][y] != 0)
                    {
                        throw Exception("Medium " + iMedium->name() +
                                " overlaps with " + areaMatrix[x][y]->name()
                                );
                    }
                    else
                    {
                        areaMatrix[x][y] = &*iMedium;
                    }
                }
            }
        }   // for: by medium areas
    }   // for: by mediums


    // Pasitikrinam, ar neliko matricos langeliu, kuriems nepriskirta jokia medziaga.
    for (int x = 0; x < areaMatrixX; x++)
    {
        for (int y = 0; y < areaMatrixY; y++)
        {
            if (areaMatrix[x][y] == 0)
            {
                char buf[256];
                sprintf(buf, "No medium assined to cell x=%i y=%i", x, y);
                throw Exception(buf);
            }
        }
    }


    // TODO: Tolia krastines salygos ir t.t.
    LOG4CXX_DEBUG(logger, "analyze() DONE");
}



/**
 *  Grazina tasko indeksa asyje.
 *  \param axis Kur ieskoti.
 *  \param name Ko ieskoti.
 *  \throws Exception If symbol not found in axis point set.
 */
int cfg::ConfigAnalyzer2D::boundIndexInAxis(
        const xsd::model::Axis          &axis,
        const xsd::model::SymbolName    &name
        )
{
    for (int i = 0; i < axis.point().size(); i++)
        if (axis.point()[i].position() == name)
            return i;

    throw Exception("Symbol " + name + " not found in axis " + axis.name().get());
}



/**
 *  Grazina substancijos indeksa.
 *  \param substance    Ko ieskoti (lygina pagal name).
 *  \return Indeksas.
 *  \throws Exception If substance not found.
 */
int cfg::ConfigAnalyzer2D::substanceIndex(
        const ::xsd::model::Substance &substance
        )
{
    for (int i = 0; i < model->substance().size(); i++)
        if (model->substance()[i].name() == substance.name())
            return i;

    throw Exception("Substance " + substance.name() + " not found in model");
}



/* ************************************************************************** */
/* ************************************************************************** */
