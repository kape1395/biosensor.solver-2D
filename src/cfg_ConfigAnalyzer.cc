#include "cfg_ConfigAnalyzer.hh"
#include <cstdio>
#include "Exceptions.hh"
#include "xsd/Model.hh"
#include "xsd/ModelMediumReaction.hh"
#include "xsd/ModelBoundCondition.hh"


/* ************************************************************************** */
/* ************************************************************************** */



/* ************************************************************************** */
/**
 *  Konstruktorius.
 *  \param model    Modelis, kuri analizuoti.
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



/* ************************************************************************** */
/**
 *  Destruktorius.
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

    if (boundConditionH != 0)
    {
        for (int x = 0; x < areaMatrixX; x++)
        {
            for (int y = 0; y <= areaMatrixY; y++)
                delete[] boundConditionH[x][y];
            delete[] boundConditionH[x];
        }
        delete[] boundConditionH;
    }

    if (boundConditionV != 0)
    {
        for (int x = 0; x <= areaMatrixX; x++)
        {
            for (int y = 0; y < areaMatrixY; y++)
                delete[] boundConditionV[x][y];
            delete[] boundConditionV[x];
        }
        delete[] boundConditionV;
    }

    // Nepamirstam istrinti ir automatiskai sugeneruotu krastiniu salygu.
    while (!generatedBoundConditions.empty())
    {
        delete generatedBoundConditions.back();
        generatedBoundConditions.pop_back();
    }

}



/* ************************************************************************** */
/**
 *  Vykdo analize, susideda viska i patogesnes strukturas, validuoja kai
 *  kurias konfiguracijos vietas.
 *
 *  \throws Exception
 */
void cfg::ConfigAnalyzer2D::analyze()
{
    LOG4CXX_DEBUG(logger, "analyze()");


    //  Patikrinam, ar sita klase palaiko nurodyta koordinaciu sistema.
    if (model->coordinateSystem() != xsd::model::CoordinateSystem::Cartesian &&
        model->coordinateSystem() != xsd::model::CoordinateSystem::Cylindrical)
        throw Exception(model->coordinateSystem() +
                        " coordinate system" +
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
    }


    substanceCount = model->substance().size();


    xsd::model::Axis* axisX = &model->axis()[0];
    xsd::model::Axis* axisY = &model->axis()[1];


    //  Susikuriam matrica, i kurios "langeliams" veliau priskirinesime medziagas.
    areaMatrixX = axisX->point().size() - 1;
    areaMatrixY = axisY->point().size() - 1;
    areaMatrix = new xsd::model::Medium**[areaMatrixX];
    for (int x = 0; x < areaMatrixX; x++)
    {
        areaMatrix[x] = new xsd::model::Medium*[areaMatrixY];
        for (int y = 0; y < areaMatrixY; y++)
            areaMatrix[x][y] = 0;
    }

    //  Dydis: boundConditionH = [areaMatrixX][areaMatrixY+1][substanceCount]
    boundConditionH = new xsd::model::BoundCondition***[areaMatrixX];
    for (int x = 0; x < areaMatrixX; x++)
    {
        boundConditionH[x] = new xsd::model::BoundCondition**[areaMatrixY + 1];
        for (int y = 0; y <= areaMatrixY; y++)
        {
            boundConditionH[x][y] = new xsd::model::BoundCondition*[substanceCount];
            for (int s = 0; s < substanceCount; s++)
                boundConditionH[x][y][s] = 0;
        }
    }

    //  Dydis: boundConditionV = [areaMatrixX + 1][areaMatrixY][substanceCount]
    boundConditionV = new xsd::model::BoundCondition***[areaMatrixX + 1];
    for (int x = 0; x <= areaMatrixX; x++)
    {
        boundConditionV[x] = new xsd::model::BoundCondition**[areaMatrixY];
        for (int y = 0; y < areaMatrixY; y++)
        {
            boundConditionV[x][y] = new xsd::model::BoundCondition*[substanceCount];
            for (int s = 0; s < substanceCount; s++)
                boundConditionV[x][y][s] = 0;
        }
    }



    ////////////////////////////////////////////////////////////////////////////
    // Sudeliojame sritis i masyva, patikrinam ar visos sritys padengtos
    //


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

    // Sudeliojame sritis i masyva, patikrinam ar visos sritys padengtos
    ////////////////////////////////////////////////////////////////////////////


    ////////////////////////////////////////////////////////////////////////////
    // Sudeliojame krastines salygas
    //

    // Cia sudeliosime krastines salygas, kurios nurodytos konfiguracijoje.
    for (::xsd::model::Model::bound_iterator iBound = model->bound().begin();
            iBound != model->bound().end(); iBound++)
    {
        bool vertical;
        int  indexAt;
        int  indexFrom;
        int  indexTo;

        //  Nustatom, kokia kryptimi nurodytas krastas.
        try
        {
            indexAt = boundIndexInAxis(*axisX, iBound->at());
            vertical = true;
        }
        catch (Exception&)
        {
            try
            {
                indexAt = boundIndexInAxis(*axisY, iBound->at());
                vertical = false;
            }
            catch (Exception&)
            {
                throw Exception("Bound at=" + iBound->at() + " not found in both axes");
            }
        }

        // Patikrinam, ar paduota from/to ir pasiimam indeksus tu tasku, asyse.
        if (!iBound->from().present() || !iBound->to().present())
        {
            throw Exception("Attributes from, to and at must be specified for 2D model");
        }
        indexFrom = boundIndexInAxis(*(vertical ? axisY : axisX), iBound->from().get());
        indexTo   = boundIndexInAxis(*(vertical ? axisY : axisX), iBound->to  ().get());

        // Einam per ta atkarpa, kuri nurodyta krasto aprasyme.
        for (int i = indexFrom; i < indexTo; i++)
        {
            int x = vertical ? indexAt : i;
            int y = vertical ? i : indexAt;
            ::xsd::model::BoundCondition**** boundCondition = vertical ? boundConditionV : boundConditionH;

            // Pereinam per visas nurodytas salygas, ir priskiriam jas atitinkamoms substancijoms
            for (::xsd::model::Bound::condition_iterator iCond = iBound->condition().begin();
                    iCond != iBound->condition().end(); iCond++)
            {
                boundCondition[x][y][substanceIndex(iCond->substance())] = &*iCond;
            }
        }

    }   // for: by bounds


    // Cia ieskosime vietu, kuriose krastines salygos nepriskirtos ir bandysi
    // atspeti, kas gi cia turetu buti. (Horizontalios krastines salygos).
    for (int x = 0; x < areaMatrixX; x++)
    {
        for (int y = 0; y <= areaMatrixY; y++)
        {
            for (int s = 0; s < substanceCount; s++)
            {
                if (boundConditionH[x][y][s] == 0)
                {
                    boundConditionH[x][y][s] = calculateBoundCondition(
                        y == 0           ? 0 : areaMatrix[x][y - 1],
                        y == areaMatrixY ? 0 : areaMatrix[x][y    ],
                        s);
                }
            }
        }
    }

    // Cia ieskosime vietu, kuriose krastines salygos nepriskirtos ir bandysi
    // atspeti, kas gi cia turetu buti. (Vertikalios krastines salygos).
    for (int x = 0; x <= areaMatrixX; x++)
    {
        for (int y = 0; y < areaMatrixY; y++)
        {
            for (int s = 0; s < substanceCount; s++)
            {
                if (boundConditionV[x][y][s] == 0)
                {
                    boundConditionV[x][y][s] = calculateBoundCondition(
                        x == 0           ? 0 : areaMatrix[x - 1][y],
                        x == areaMatrixX ? 0 : areaMatrix[x    ][y],
                        s);
                }
            }
        }
    }

    // Sudeliojame krastines salygas
    ////////////////////////////////////////////////////////////////////////////


    LOG4CXX_DEBUG(logger, "analyze() DONE");
}



/* ************************************************************************** */
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

    throw Exception("Symbol " + name + " not found in axis " + axis.name());
}



/* ************************************************************************** */
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
/**
 *  \returns 0 if not found.
 */
const ::xsd::model::MediumDiffusion* cfg::ConfigAnalyzer2D::diffusionBySubstance(
        const ::xsd::model::Medium&    medium,
        const ::xsd::model::Substance& substance
        )
{
    for (int i = 0; i < medium.diffusion().size(); i++)
        if (medium.diffusion()[i].substance() == substance.name())
            return &medium.diffusion()[i];

    return 0;
}



/* ************************************************************************** */
/**
 *  Parenka krastine salyga pagal gretimas sritis.
 *  \throws Exception Jei neaisku ka parinkti.
 */
xsd::model::BoundCondition* cfg::ConfigAnalyzer2D::calculateBoundCondition(
    xsd::model::Medium* medium1,
    xsd::model::Medium* medium2,
    int substanceIdx
    )
{
    xsd::model::BoundCondition* bc = 0;
    const xsd::model::Substance*      substance = &model->substance()[substanceIdx];
    const xsd::model::MediumDiffusion* diff1 = medium1 == 0 ? 0 : diffusionBySubstance(*medium1, *substance);
    const xsd::model::MediumDiffusion* diff2 = medium2 == 0 ? 0 : diffusionBySubstance(*medium2, *substance);

    if (diff1 == 0 && diff2 == 0)
    {
        bc = new ::xsd::model::bc::Null(substance->name());
    }
    else if (diff1 == 0 || diff2 == 0)
    {
        bc = new ::xsd::model::bc::Wall(substance->name());
    }
    else
    {
        bc = new ::xsd::model::bc::Merge(substance->name());
    }

    if (bc == 0)
        throw Exception("I dont know how to calculate bound condition...");

    generatedBoundConditions.push_back(bc);
    return bc;
}



/* ************************************************************************** */
/* ************************************************************************** */
