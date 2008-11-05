#include "StructureAnalyzer.hxx"
#include "../Exception.hxx"


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_CFG_NS::StructureAnalyzer::analyze(BIO_XML_NS::model::Model* config)
{

    typedef BIO_XML_NS::model::Model::axis_iterator it_axis;
    typedef BIO_XML_NS::model::Model::substance_iterator it_subst;
    typedef BIO_XML_NS::model::Model::medium_iterator it_med;
    typedef BIO_XML_NS::model::Medium::area_iterator it_area;
    typedef BIO_XML_NS::model::Medium::diffusion_iterator it_diff;
    typedef BIO_XML_NS::model::Medium::reaction_iterator it_reac;


    LOG4CXX_INFO(log, "cleanup...");

    ////////////////////////////////////////////////////////////////////////////
    //  Release all old data.
    //
    twoDimensional = false;
    for (int h = 0; h < pointsH.size(); h++)
    {
        for (int v = 0; v < pointsV.size(); v++)
        {
            delete [] diffusions[h][v];
            delete [] initialConcentrations[h][v];
        }
        delete [] reactions[h];
        delete [] diffusions[h];
        delete [] initialConcentrations[h];
        delete [] mediums[h];
    }
    if (reactions)
    {
        delete [] reactions;
        reactions = 0;
    }
    if (diffusions)
    {
        delete [] diffusions;
        diffusions = 0;
    }
    if (initialConcentrations)
    {
        delete [] initialConcentrations;
        initialConcentrations = 0;
    }
    if (mediums)
    {
        delete [] mediums;
        mediums = 0;
    }
    pointsH.clear();
    pointsV.clear();
    substances.clear();
    //
    //  Release all old data.
    ////////////////////////////////////////////////////////////////////////////

    LOG4CXX_INFO(log, "cleanup... Done");


    //  If 0 is passed as config, only cleanup should be performed.
    this->config = config;
    if (config == 0)
        return;


    LOG4CXX_INFO(log, "analyze...");

    ////////////////////////////////////////////////////////////////////////////
    //  Fill pointsH and pointsV
    //
    if (config->coordinateSystem() == BIO_XML_NS::model::CoordinateSystem::Cartesian)
    {
        LOG4CXX_DEBUG(log, "Found coordinate system \"Cartesian\", axes will be x and y.");
        this->twoDimensional = true;
        bool found_h = false;
        bool found_v = false;
        for (it_axis axis = config->axis().begin(); axis < config->axis().end(); axis++)
        {
            if (axis->name() == "x")
            {
                fillListWithAxisPoints(pointsH, *axis);
                found_h = true;
            }
            if (axis->name() == "y")
            {
                fillListWithAxisPoints(pointsV, *axis);
                found_v = true;
            }

        }
        if (!(found_h && found_v))
        {
            LOG4CXX_ERROR(log, "Both axes \"x\" and \"y\" are needed but some of them not found.");
            throw Exception("Missing axis");
        }
    }
    else if (config->coordinateSystem() == BIO_XML_NS::model::CoordinateSystem::Cylindrical)
    {
        LOG4CXX_DEBUG(log, "Found coordinate system \"Cylindrical\", axes will be r and z.");
        this->twoDimensional = true;
        bool found_h = false;
        bool found_v = false;
        for (it_axis axis = config->axis().begin(); axis < config->axis().end(); axis++)
        {
            if (axis->name() == "r")
            {
                fillListWithAxisPoints(pointsH, *axis);
                found_h = true;
            }
            if (axis->name() == "z")
            {
                fillListWithAxisPoints(pointsV, *axis);
                found_v = true;
            }

        }
        if (!(found_h && found_v))
        {
            LOG4CXX_ERROR(log, "Both axes \"r\" and \"z\" are needed but some of them not found.");
            throw Exception("Missing axis");
        }
    }
    else if (config->coordinateSystem() == BIO_XML_NS::model::CoordinateSystem::Linear)
    {
        LOG4CXX_DEBUG(log, "Found coordinate system \"Linear\", axis will be x.");
        this->twoDimensional = false;
        bool found = false;
        for (it_axis axis = config->axis().begin(); axis < config->axis().end(); axis++)
        {
            if (axis->name() == "x")
            {
                fillListWithAxisPoints(pointsH, *axis);
                found = true;
            }
        }
        if (!found)
        {
            LOG4CXX_ERROR(log, "Axis  \"x\" is needed but not found.");
            throw Exception("Missing axis");
        }
        pointsV.push_back(&axisPoint0);
    }
    else
    {
        LOG4CXX_ERROR(log, "I dont know coordinate system, specified in the configuration.");
        throw Exception("Unknown coordinate system.");
    }
    //
    //  Fill pointsH and pointsV
    ////////////////////////////////////////////////////////////////////////////


    ////////////////////////////////////////////////////////////////////////////
    //  Fill substances (order is preserved from config file).
    //
    for (it_subst subst = config->substance().begin(); subst < config->substance().end(); subst++)
    {
        substances.push_back(&*subst);
    }
    //
    //  Fill substances.
    ////////////////////////////////////////////////////////////////////////////


    ////////////////////////////////////////////////////////////////////////////
    //  Fill mediums.
    //
    mediums = new BIO_XML_NS::model::Medium* *[pointsH.size()];
    for (int h = 0; h < pointsH.size(); h++)
    {
        mediums[h] = new BIO_XML_NS::model::Medium*[pointsV.size()];
        for (int v = 0; v < pointsV.size(); v++)
        {
            mediums[h][v] = 0;
        }
    }
    for (it_med medium = config->medium().begin(); medium < config->medium().end(); medium++)
    {
        for (it_area area = medium->area().begin(); area < medium->area().end(); area++)
        {
            int h1 = -1;
            int h2 = -1;
            int v1 = -1;
            int v2 = -1;
            if (twoDimensional)
            {
                h1 = area->left  ().present() ? getPointIndexInAxis(pointsH, *area->left  ()) : h1;
                h2 = area->right().present() ? getPointIndexInAxis(pointsH, *area->right()) : h2;
                v1 = area->top   ().present() ? getPointIndexInAxis(pointsV, *area->top   ()) : v1;
                v2 = area->bottom().present() ? getPointIndexInAxis(pointsV, *area->bottom()) : v2;
            }
            else
            {
                h1 = area->from().present() ? getPointIndexInAxis(pointsH, *area->from()) : h1;
                h2 = area->to()  .present() ? getPointIndexInAxis(pointsH, *area->to  ()) : h2;
                v1 = 0;
                v2 = 1;
            }
            if (h1 == -1 || h2 == -1 || v1 == -1 || v2 == -1)
            {
                LOG4CXX_ERROR(log, "Some of needed coordinates are not specified in medium`s area");
                throw Exception("Invalid configuration");
            }
            for (int h = h1; h < h2; h++)
            {
                for (int v = v1; v < v2; v++)
                {
                    mediums[h][v] = &*medium;
                }
            }
        }
    }
    //
    //  Fill mediums.
    ////////////////////////////////////////////////////////////////////////////


    ////////////////////////////////////////////////////////////////////////////
    //  Fill diffusions and reactions.
    //
    reactions = new std::vector< BIO_XML_NS::model::MediumReaction* >*[pointsH.size()];
    diffusions = new BIO_XML_NS::model::Symbol* **[pointsH.size()];
    initialConcentrations = new BIO_XML_NS::model::Symbol* **[pointsH.size()];
    for (int h = 0; h < pointsH.size(); h++)
    {
        reactions[h] = new std::vector< BIO_XML_NS::model::MediumReaction* >[pointsV.size()];
        diffusions[h] = new BIO_XML_NS::model::Symbol* *[pointsV.size()];
        initialConcentrations[h] = new BIO_XML_NS::model::Symbol* *[pointsV.size()];
        for (int v = 0; v < pointsV.size(); v++)
        {
            diffusions[h][v] = new BIO_XML_NS::model::Symbol*[substances.size()];
            initialConcentrations[h][v] = new BIO_XML_NS::model::Symbol*[substances.size()];
            for (int s = 0; s < substances.size(); s++)
            {
                diffusions[h][v][s] = 0;
                initialConcentrations[h][v][s] = 0;
            }
            if (mediums[h][v])
            {
                for (it_diff diffusion = mediums[h][v]->diffusion().begin(); diffusion < mediums[h][v]->diffusion().end(); diffusion++)
                {
                    int substIdx = getSubstanceIndex(diffusion->substance());
                    diffusions[h][v][substIdx] = getSymbol(diffusion->coefficient());
                    initialConcentrations[h][v][substIdx] = getSymbol(diffusion->initial());
                }
                for (it_reac reaction = mediums[h][v]->reaction().begin(); reaction < mediums[h][v]->reaction().end(); reaction++)
                {
                    reactions[h][v].push_back(&*reaction);
                }
            }
        }
    }
    //
    //  Fill diffusions and reactions.
    ////////////////////////////////////////////////////////////////////////////

    LOG4CXX_INFO(log, "analyze... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_XML_NS::model::Symbol* BIO_CFG_NS::StructureAnalyzer::getSymbol(std::string& name)
{
    for (int i = 0; i < config->symbol().size(); i++)
    {
        if (config->symbol()[i].name() == name)
            return &config->symbol()[i];
    }
    return 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_CFG_NS::StructureAnalyzer::fillListWithAxisPoints(std::vector< BIO_XML_NS::model::Symbol* >& list,
        BIO_XML_NS::model::Axis& axis)
{
    typedef BIO_XML_NS::model::Axis::point_iterator it_point;
    for (it_point p = axis.point().begin(); p < axis.point().end(); p++)
    {
        list.push_back(getSymbol(p->position()));
    }

}


/* ************************************************************************** */
/* ************************************************************************** */
int BIO_CFG_NS::StructureAnalyzer::getPointIndexInAxis(std::vector< BIO_XML_NS::model::Symbol* >& axis, std::string& pointSymbolName)
{
    int i = 0;
    for (std::vector<BIO_XML_NS::model::Symbol*>::iterator it = axis.begin(); it < axis.end(); it++, i++)
    {
        if ((*it)->name() == pointSymbolName)
            return i;
    }
    LOG4CXX_ERROR(log, "Point, specified in medium area is not found in corresponding area.");
    throw Exception("Invalid configuration");
}


/* ************************************************************************** */
/* ************************************************************************** */
int BIO_CFG_NS::StructureAnalyzer::getSubstanceIndex(std::string& substanceName)
{
    int i = 0;
    for (std::vector< BIO_XML_NS::model::Substance* >::iterator it = substances.begin(); it < substances.end(); it++, i++)
    {
        if ((*it)->name() == substanceName)
            return i;
    }
    LOG4CXX_ERROR(log, "Substance nor found by name.");
    throw Exception("Invalid configuration");
}


/* ************************************************************************** */
/* ************************************************************************** */
