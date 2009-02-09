#ifndef BIO_CFG_BoundAnalyzer_HXX
#define BIO_CFG_BoundAnalyzer_HXX
#include "../../biosensor.hxx"
#include "StructureAnalyzer.hxx"
#include <biosensor-xml.hxx>
#include <vector>
#include <log4cxx/logger.h>
BIO_CFG_NS_BEGIN


/**
 *  Analyzer, that provides all information, related to boundary conditions
 *  of the model. As an input it uses StructureAnalyzer.
 */
class BoundAnalyzer
{
private:
    log4cxx::LoggerPtr log;

    /**
     *  Internal...
     */
    struct BoundSubstanceInfo
    {
        BIO_XML_NS::model::Bound* bound;
        BIO_XML_NS::model::BoundSubstance* boundSubstance;
        BoundSubstanceInfo()
        {
            bound = 0;
            boundSubstance = 0;
        }
        BoundSubstanceInfo& operator = (BoundSubstanceInfo& source)
        {
            bound = source.bound;
            boundSubstance = source.boundSubstance;
            return *this;
        }
    };

    /**
     *  Analyzer, used to get all needed info about the model.
     */
    StructureAnalyzer *structAnalyzer;

    /**
     *  Array of bound conditions, structure is (BoundSubstance*[h][v][s][side]).
     */
    BoundSubstanceInfo* **** boundSubstances;

    int sizeH;  //!< #boundSubstances size in h (number of areas horizontally)
    int sizeV;  //!< #boundSubstances size in v (number of areas vertically)
    int sizeS;  //!< #boundSubstances size in s

    /**
     *  A vector for the bounds, allocated by this class.
     *  It is used for a cleanup.
     */
    std::vector< BIO_XML_NS::model::BoundSubstance* > allocatedBoundConditions;

public:

    enum AreaSide
    {
        TOP     = 0,
        BOTTOM  = 1,
        LEFT    = 2,
        RIGHT   = 3
    };

    /**
     *  Constructor.
     *  \param structAnalyzer Struture analyzer, created for this config.
     */
    BoundAnalyzer(StructureAnalyzer *structAnalyzer);

    /**
     *  Destructor.
     */
    virtual ~BoundAnalyzer();

    /**
     *  Get bound specification for specific area side and substance.
     *
     *  \param s    Substance index as returned by StructureAnalyzer::getSubstanceIndex
     *  \param h    Horizontal position of the area.
     *  \param v    Vertical position of the area.
     *  \param side Side of the area.
     *  \return     Bound specification for particular substance.
     */
    BIO_XML_NS::model::BoundSubstance* getBoundForSubstance(int s, int h, int v, AreaSide side);

    /**
     *  Returns the name of the bound, that defined specified bound condition.
     *
     *  \param s    Substance index as returned by StructureAnalyzer::getSubstanceIndex
     *  \param h    Horizontal position of the area.
     *  \param v    Vertical position of the area.
     *  \param side Side of the area.
     *  \return Name of the bound. 0 is returned is a name was not specified or
     *          a bound condition was generated (guessed).
     */
    std::string* getBoundName(int s, int h, int v, AreaSide side);

private:

    /**
     *  Internal...
     */
    void applyBoundCondition(
        int h,
        int v,
        int s,
        AreaSide side,
        BIO_XML_NS::model::Bound* bProvided,
        BIO_XML_NS::model::BoundSubstance* bsProvided
    );

};


BIO_CFG_NS_END
#endif
