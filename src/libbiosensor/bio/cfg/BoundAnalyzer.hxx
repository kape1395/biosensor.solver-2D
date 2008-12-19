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
     *  Analyzer, used to get all needed info about the model.
     */
    StructureAnalyzer *structAnalyzer;

    /**
     *  Array of bound conditions, structure is (BoundSubstance*[h][v][s][side]).
     */
    BIO_XML_NS::model::BoundSubstance* **** bounds;

    int sizeH;  //!< #bounds size in h
    int sizeV;  //!< #bounds size in v
    int sizeS;  //!< #bounds size in s

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
    BIO_XML_NS::model::BoundSubstance* getBound(int s, int h, int v, AreaSide side);

private:

    /**
     *  Internal...
     */
    void applyBoundCondition(
        int h,
        int v,
        int s,
        AreaSide side,
        BIO_XML_NS::model::BoundSubstance* bsProvided
    );

};


BIO_CFG_NS_END
#endif
