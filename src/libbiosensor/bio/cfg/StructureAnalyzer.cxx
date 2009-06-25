
#include "Model.hxx"

#include "StructureAnalyzer.hxx"
#include "../Exception.hxx"
#include "../Logging.hxx"
#include <algorithm>
#define LOGGER "libbiosensor::StructureAnalyzer: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_CFG_NS::StructureAnalyzer::StructureAnalyzer(
    BIO_XML_MODEL_NS::Model* config
) :
        axisPoint0(BIO_XML_MODEL_NS::SymbolName("axisPoint0"), 0.0),
        diffusion0(BIO_XML_MODEL_NS::SymbolName("diffusion0"), 0.0)
{
    axisPoint0.dimension("m");
    diffusion0.dimension("m^2/s");
    twoDimensional = false; // it is not very correct, but...
    reactions = 0;
    diffusions = 0;
    initialConcentrations = 0;
    mediums = 0;

    typedef BIO_XML_MODEL_NS::Model::axis_iterator it_axis;
    typedef BIO_XML_MODEL_NS::Model::substance_iterator it_subst;
    typedef BIO_XML_MODEL_NS::Model::medium_iterator it_med;
    typedef BIO_XML_MODEL_NS::Medium::area_iterator it_area;
    typedef BIO_XML_MODEL_NS::Medium::substance_iterator it_diff;
    typedef BIO_XML_MODEL_NS::Medium::reaction_iterator it_reac;

    LOG_INFO(LOGGER << "StructureAnalyzer()...");

    this->config = config;

    ////////////////////////////////////////////////////////////////////////////
    //  Fill pointsH and pointsV
    //  NOTE About axes: Vertical axis is transformed in such way, that it has
    //      direction "down", but starts at top-most point of the model.
    //      So v==0 means top bound of the modelled area.
    //  NOTE AGAIN: Lets try to do all in such, way that axis directionsare
    //      used as in math, i.e. vertical axis goes from bottom to top.
    //
    if (config->coordinateSystem() == BIO_XML_MODEL_NS::CoordinateSystem::Cartesian)
    {
        LOG_DEBUG(LOGGER << "Found coordinate system \"Cartesian\", axes will be x and y.");
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
            LOG_ERROR(LOGGER << "Both axes \"x\" and \"y\" are needed but some of them not found.");
            throw Exception("Missing axis");
        }
    }
    else if (config->coordinateSystem() == BIO_XML_MODEL_NS::CoordinateSystem::Cylindrical)
    {
        LOG_DEBUG(LOGGER << "Found coordinate system \"Cylindrical\", axes will be r and z.");
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
            LOG_ERROR(LOGGER << "Both axes \"r\" and \"z\" are needed but some of them not found.");
            throw Exception("Missing axis");
        }
    }
    else if (config->coordinateSystem() == BIO_XML_MODEL_NS::CoordinateSystem::Linear)
    {
        LOG_DEBUG(LOGGER << "Found coordinate system \"Linear\", axis will be x.");
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
            LOG_ERROR(LOGGER << "Axis  \"x\" is needed but not found.");
            throw Exception("Missing axis");
        }
        pointsV.push_back(&axisPoint0);
    }
    else
    {
        LOG_ERROR(LOGGER << "I dont know coordinate system, specified in the configuration.");
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
    mediums = new BIO_XML_MODEL_NS::Medium* *[pointsH.size()];
    for (unsigned h = 0; h < pointsH.size(); h++)
    {
        mediums[h] = new BIO_XML_MODEL_NS::Medium*[pointsV.size()];
        for (unsigned v = 0; v < pointsV.size(); v++)
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
                LOG_ERROR(LOGGER << "Some of needed coordinates are not specified in medium`s area");
                throw Exception("Invalid configuration");
            }
            if (h1 > h2)
            {
                LOG_WARN(LOGGER << "Swapping horizontal boundary points for area definition....");
                std::swap<int>(h1, h2);
            }
            if (v1 > v2)    // Because vertical axis has direction "down".
            {
                LOG_WARN(LOGGER << "Swapping vertical boundary points for area definition....");
                std::swap<int>(v1, v2);
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
    reactions = new std::vector< BIO_XML_MODEL_NS::Reaction* >*[pointsH.size()];
    diffusions = new BIO_XML_MODEL_NS::Symbol* **[pointsH.size()];
    initialConcentrations = new BIO_XML_MODEL_NS::Symbol* **[pointsH.size()];
    for (unsigned h = 0; h < pointsH.size(); h++)
    {
        reactions[h] = new std::vector< BIO_XML_MODEL_NS::Reaction* >[pointsV.size()];
        diffusions[h] = new BIO_XML_MODEL_NS::Symbol* *[pointsV.size()];
        initialConcentrations[h] = new BIO_XML_MODEL_NS::Symbol* *[pointsV.size()];
        for (unsigned v = 0; v < pointsV.size(); v++)
        {
            diffusions[h][v] = new BIO_XML_MODEL_NS::Symbol*[substances.size()];
            initialConcentrations[h][v] = new BIO_XML_MODEL_NS::Symbol*[substances.size()];
            for (unsigned s = 0; s < substances.size(); s++)
            {
                diffusions[h][v][s] = 0;
                initialConcentrations[h][v][s] = 0;
            }
            if (mediums[h][v])
            {
                for (it_diff diffusion = mediums[h][v]->substance().begin(); diffusion < mediums[h][v]->substance().end(); diffusion++)
                {
                    int substIdx = getSubstanceIndex(diffusion->name());
                    diffusions[h][v][substIdx] = (diffusion->diffusion().present())
                                                 ? getSymbol(diffusion->diffusion().get())
                                                 : &diffusion0;
                    initialConcentrations[h][v][substIdx] = getSymbol(diffusion->initial());
                }
                for (it_reac reaction = mediums[h][v]->reaction().begin(); reaction < mediums[h][v]->reaction().end(); reaction++)
                {
                    reactions[h][v].push_back(getReaction(reaction->name()));
                }
            }
        }
    }
    //
    //  Fill diffusions and reactions.
    ////////////////////////////////////////////////////////////////////////////

    LOG_INFO(LOGGER << "StructureAnalyzer()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_CFG_NS::StructureAnalyzer::~StructureAnalyzer()
{
    LOG_INFO(LOGGER << "~StructureAnalyzer()...");

    twoDimensional = false;
    for (unsigned h = 0; h < pointsH.size(); h++)
    {
        for (unsigned v = 0; v < pointsV.size(); v++)
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

    config = 0;

    LOG_INFO(LOGGER << "~StructureAnalyzer()... Done");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_XML_MODEL_NS::MediumName* BIO_CFG_NS::StructureAnalyzer::getMediumName(int h, int v)
{
    if (!mediums[h][v])
        return 0;

    return &mediums[h][v]->name();
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_XML_MODEL_NS::Symbol* BIO_CFG_NS::StructureAnalyzer::getSymbol(BIO_XML_MODEL_NS::SymbolName& name)
{
    for (unsigned i = 0; i < config->symbol().size(); i++)
    {
        if (config->symbol()[i].name() == name)
            return &config->symbol()[i];
    }
    return 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_CFG_NS::StructureAnalyzer::fillListWithAxisPoints(std::vector< BIO_XML_MODEL_NS::Symbol* >& list,
        BIO_XML_MODEL_NS::Axis& axis,
        bool invert
                                                          )
{
    typedef BIO_XML_MODEL_NS::Axis::point_iterator it_point;
    for (it_point p = axis.point().begin(); p != axis.point().end(); p++)
    {
        if (invert)
        {
            list.insert(list.begin(), getSymbol(p->position()));
        }
        else
        {
            list.push_back(getSymbol(p->position()));
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
bool BIO_CFG_NS::StructureAnalyzer::isPointInAxis(
    Axis axis,
    BIO_XML_MODEL_NS::SymbolName& pointSymbolName
)
{
    std::vector< BIO_XML_MODEL_NS::Symbol* >& points = (axis == HORIZONTAL) ? pointsH : pointsV;

    for (std::vector<BIO_XML_MODEL_NS::Symbol*>::iterator it = points.begin(); it < points.end(); it++)
    {
        if ((*it)->name() == pointSymbolName)
            return true;
    }
    return false;
}


/* ************************************************************************** */
/* ************************************************************************** */
int BIO_CFG_NS::StructureAnalyzer::getPointIndexInAxis(
    Axis axis,
    BIO_XML_MODEL_NS::SymbolName& pointSymbolName
)
{
    return getPointIndexInAxis(
               (axis == HORIZONTAL) ? pointsH : pointsV,
               pointSymbolName
           );
}


/* ************************************************************************** */
/* ************************************************************************** */
int BIO_CFG_NS::StructureAnalyzer::getPointIndexInAxis(
    std::vector< BIO_XML_MODEL_NS::Symbol* >& axis,
    std::string& pointSymbolName
)
{
    int i = 0;
    for (std::vector<BIO_XML_MODEL_NS::Symbol*>::iterator it = axis.begin(); it < axis.end(); it++, i++)
    {
        if ((*it)->name() == pointSymbolName)
            return i;
    }
    LOG_ERROR(LOGGER << "Point, specified in medium area is not found in corresponding area.");
    throw Exception("Invalid configuration");
}


/* ************************************************************************** */
/* ************************************************************************** */
int BIO_CFG_NS::StructureAnalyzer::getSubstanceIndex(BIO_XML_MODEL_NS::SubstanceName& substanceName)
{
    int i = 0;
    for (std::vector< BIO_XML_MODEL_NS::Substance* >::iterator it = substances.begin(); it < substances.end(); it++, i++)
    {
        if ((*it)->name() == substanceName)
            return i;
    }
    LOG_ERROR(LOGGER << "Substance nor found by name.");
    throw Exception("Invalid configuration");
}


/* ************************************************************************** */
/* ************************************************************************** */
std::vector<int> BIO_CFG_NS::StructureAnalyzer::getSubstanceIndexesInArea(int h, int v)
{
    std::vector<int> indexes;
    for (unsigned i = 0; i < getSubstances().size(); i++)
    {
        BIO_XML_MODEL_NS::Symbol *sym = getDiffusion(i, h, v);
        if (sym != 0)
        {
            indexes.push_back(i);
        }
    }
    return indexes;
}


/* ************************************************************************** */
/* ************************************************************************** */
std::vector<int> BIO_CFG_NS::StructureAnalyzer::getSubstanceIndexesInMedium(BIO_XML_MODEL_NS::MediumName& name)
{
    typedef BIO_XML_MODEL_NS::Model::medium_iterator it_m;
    typedef BIO_XML_MODEL_NS::Medium::substance_iterator it_ms;
    std::vector<int> indexes;

    for (it_m m = config->medium().begin(); m < config->medium().end(); m++)
    {
        if (m->name().compare(name) == 0)
        {
            for (it_ms ms = m->substance().begin(); ms < m->substance().end(); ms++)
            {
                indexes.push_back(getSubstanceIndex(ms->name()));
            }
        }
    }

    std::sort(indexes.begin(), indexes.end());

    return indexes;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_XML_MODEL_NS::Reaction* BIO_CFG_NS::StructureAnalyzer::getReaction(BIO_XML_MODEL_NS::ReactionName& name)
{
    typedef BIO_XML_MODEL_NS::Model::reaction_iterator it_reac;
    for (it_reac r = config->reaction().begin(); r < config->reaction().end(); r++)
    {
        if (r->name().compare(name) == 0)
        {
            return &*r;
        }
    }
    return 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
