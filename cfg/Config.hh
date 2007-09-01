#ifndef CFG_Config_HH
#define CFG_Config_HH
#include <string>
#include <list>
namespace cfg
{



class Config;
class Substance;
class Reaction;
class Medium;
class MichaelisMentenReaction;
class DimensionPart;
class ConstantDimensionPart;
class BilinearDimensionPart;
class Area;
class Bound;



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Biojutiklio konfiguracija.
 *  \code
 *  <config>
 *      <substance name="S"/>
 *      <substance name="P"/>
 *      <reaction name="R1" type="MichaelisMenten" substrate="S" product="P" V_max="1E-8" K_M="1E-8"/>
 *      <medium name="Enzyme">
 *          <diffusion substance="S" coefficient="1E-6"/>
 *          <diffusion substance="S" coefficient="1E-6"/>
 *          <reaction name="R1"/>
 *      </medium>
 *      <medium name="PerforatedMembrane">
 *      </medium>
 *      <dimensionX>
 *          <part lenght="1E-3" type="Constant" stepCount="1000"/>
 *          <part lenght="1E-5" type="Bilinear" startStep="0.0001" factor="0.01"/>
 *      </dimensionX>
 *      <dimensionY>
 *          <part lenght="1E-5" type="Bilinear" startStep="0.0001" factor="0.01"/>
 *          <part lenght="1E-5" type="Constant" stepCount="1000"/>
 *      </dimensionY>
 *      <area position="0 0" medium="Enzyme"/>
 *      <area position="1 0" medium="PerforatedMembrane"/>
 *      <area position="0 1" medium="Enzyme"/>
 *      <area position="1 1" medium="Enzyme"/>
 *      <bound position="0 0 top">
 *          <condition substance="S" type="Constant" concentration="7E-5"/>
 *          <condition substance="P" type="Constant" concentration="0.0"/>
 *      </bound>
 *      <bound position="0 0 left">
 *          <condition substance="S" type="Wall"/>
 *          <condition substance="P" type="Wall"/>
 *      </bound>
 *      <bound position="0 1 left">
 *          <condition substance="S" type="Wall"/>
 *          <condition substance="P" type="Wall"/>
 *      </bound>
 *      <bound position="1 1 right">
 *          <condition substance="S" type="Wall"/>
 *          <condition substance="P" type="Wall"/>
 *      </bound>
 *      <bound position="0 1 bottom">
 *          <condition substance="S" type="Wall"/>
 *          <condition substance="P" type="Electrode"/>
 *      </bound>
 *      <bound position="1 1 bottom">
 *          <condition substance="S" type="Wall"/>
 *          <condition substance="P" type="Electrode"/>
 *      </bound>
 *  </config>
 *  \endcode
 */
class Config
{
protected:
    std::list<Substance*>       substances;
    std::list<Reaction*>        reactions;
    std::list<Medium*>          mediums;
    std::list<DimensionPart*>   dimensionXParts;
    std::list<DimensionPart*>   dimensionYParts;
    std::list<Area*>            areas;
    std::list<Bound*>           bounds;

public:
    Config()
    {}

    virtual ~Config();

    virtual std::list<Substance*>& getSubstances()
    {
        return substances;
    }

    virtual std::list<Reaction*>& getReactions()
    {
        return reactions;
    }

    virtual std::list<Medium*>& getMediums()
    {
        return mediums;
    }

    virtual std::list<DimensionPart*>& getDimensionXParts()
    {
        return dimensionXParts;
    }

    virtual std::list<DimensionPart*>& getDimensionYParts()
    {
        return dimensionYParts;
    }

    virtual std::list<Area*>& getAreas()
    {
        return areas;
    }

    virtual std::list<Bound*>& getBounds()
    {
        return bounds;
    }

};



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Modelyje itaka turinti medziaga, kuri gali reaguoti ar difunduoti.
 */
class Substance
{
protected:
    std::string name;

public:
    Substance(
        std::string name
    )
    {
        this->name = name;
    }

    virtual ~Substance()
    {}

    virtual std::string& getName()
    {
        return name;
    }

};



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Abstrakti reakcija.
 */
class Reaction
{
protected:
    std::string name;

public:
    Reaction(
        std::string name
    )
    {
        this->name = name;
    }

    virtual ~Reaction()
    {}

    virtual std::string& getName()
    {
        return name;
    }

    virtual std::string getType() = 0;
};



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Michaelis-Menten fermentine reakcija.
 *  Si reakcija aprasoma lygtimi: \f$ \frac{V_{max} S}{K_M + S} \f$
 */
class MichaelisMentenReaction : public Reaction
{
protected:
    Substance *substrate;   ///< Substratas (naudojasi)
    Substance *product;     ///< Produktas (gaminasi)
    double     V_max;       ///< Maksimalus reakcijos greitis.
    double     K_M;         ///< Michaelis konstanta.

public:
    MichaelisMentenReaction(
        std::string name,
        Substance   *substrate,
        Substance   *product,
        double      V_max,
        double      K_M
    ) : Reaction(name)
    {
        this->substrate = substrate;
        this->product   = product;
        this->V_max     = V_max;
        this->K_M       = K_M;
    }

    virtual ~MichaelisMentenReaction()
    {}

    virtual Substance* getSubstrate()
    {
        return substrate;
    }

    virtual Substance* getProduct()
    {
        return product;
    }

    virtual double getV_max()
    {
        return V_max;
    }

    virtual double getK_M()
    {
        return K_M;
    }

    virtual std::string getType()
    {
        return "MICHAELIS-MENTEN";
    }
};



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Medziaga, esanti kiokioje nors biojutiklio srityje.
 */
class Medium
{
public:
    class Diffusion;

protected:
    std::string             name;
    std::list<Diffusion*>   diffusions;
    std::list<Reaction*>    reactions;

public:
    Medium(std::string name)
    {
        this->name = name;
    }

    virtual ~Medium();

    virtual std::string& getName()
    {
        return name;
    }

    virtual std::list<Diffusion*>& getDiffusions()
    {
        return diffusions;
    }

    virtual std::list<Reaction*>& getReactions()
    {
        return reactions;
    }

};


/**
 *  Substancijos difuzija medziagoje.
 */
class Medium::Diffusion
{
protected:
    Substance* substance;
    double     coefficient;

public:
    Diffusion(Substance* substance, double coefficient)
    {
        this->substance   = substance;
        this->coefficient = coefficient;
    }

    virtual ~Diffusion()
    {}

    virtual Substance* getSubstance()
    {
        return substance;
    }

    virtual double getCoefficient()
    {
        return coefficient;
    }

};



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Abstrakti erdves (X/Y) dimensijos dalis.
 */
class DimensionPart
{
protected:
    double length;

public:
    DimensionPart(
        double length
    )
    {
        this->length = length;
    }

    virtual ~DimensionPart()
    {}

    virtual double getLength()
    {
        return length;
    }

    virtual std::string getType()
    {
        return "DEFAULT";
    }

};



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Dimensijos dalis, kurioje zingsniai daromi vienodi.
 */
class ConstantDimensionPart : public DimensionPart
{
protected:
    int stepCount;

public:
    ConstantDimensionPart(double length, int stepCount) : DimensionPart(length)
    {
        this->stepCount = stepCount;
    }

    virtual ~ConstantDimensionPart()
    {}

    virtual int getStepCount()
    {
        return stepCount;
    }

    virtual std::string getType()
    {
        return "CONSTANT";
    }

};



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Dimensijos dalis, kurioje zingsniai kintami, is abieju pusiu didinami i viduri.
 */
class BilinearDimensionPart : public DimensionPart
{
protected:
    double startStep;
    double factor;

public:
    BilinearDimensionPart(
        double length,
        double startStep,
        double factor
    ) : DimensionPart(length)
    {
        this->startStep = startStep;
        this->factor    = factor;
    }

    virtual ~BilinearDimensionPart()
    {}

    virtual double getStartStep()
    {
        return startStep;
    }

    virtual double getFactor()
    {
        return factor;
    }

    virtual std::string getType()
    {
        return "BILINEAR";
    }

};



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Homogeniska biojutiklio sritis.
 */
class Area
{
protected:
    int     positionX;
    int     positionY;
    Medium* medium;

public:
    Area(
        int     positionX,
        int     positionY,
        Medium* medium
    )
    {
        this->positionX = positionX;
        this->positionY = positionY;
        this->medium    = medium;
    }

    virtual ~Area()
    {}

    virtual int getPositionX()
    {
        return positionX;
    }

    virtual int getPositionY()
    {
        return positionY;
    }

    virtual Medium* getMedium()
    {
        return medium;
    }

};



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Krastine/derinimo salyga.
 */
class Bound
{
public:
    class Condition;
    class WallCondition;
    class ConstantCondition;
    class ElectrodeCondition;
    class MergeCondition;
    enum SIDE {TOP, RIGHT, BOTTOM, LEFT};

protected:
    int positionX;
    int positionY;
    int side;
    std::list<Condition*> conditions;

public:
    Bound(
        int positionX,
        int positionY,
        int side
    )
    {
        this->positionX = positionX;
        this->positionY = positionY;
        this->side      = side;
    }

    virtual ~Bound();

    virtual int getPositionX()
    {
        return positionX;
    }

    virtual int getPositionY()
    {
        return positionY;
    }

    virtual int getSide()
    {
        return side;
    }

    virtual std::list<Condition*>& getConditions()
    {
        return conditions;
    }

};


/**
 *  Konkrecios medziagos elgesys srities kraste.
 */
class Bound::Condition
{
protected:
    Substance* substance;

public:
    /// Konstruktorius.
    Condition(Substance* substance)
    {
        this->substance = substance;
    }

    /// Destruktorius.
    virtual ~Condition()
    {}

    /// Grazina medziaga, kuriai taikoma salyga.
    virtual Substance* getSubstance()
    {
        return substance;
    }

    /// Grazina salygos tipa.
    virtual std::string getType() = 0;

};


/**
 *  Nepratekejimo salyga.
 */
class Bound::WallCondition : public Bound::Condition
{
protected:

public:
    WallCondition(
        Substance* substance
    ) : Condition(substance)
    {}

    virtual ~WallCondition()
    {}

    virtual std::string getType()
    {
        return "WALL";
    }
};


/**
 *  Pastovios koncentracijos salyga.
 */
class Bound::ConstantCondition : public Bound::Condition
{
protected:
    double concentration;

public:
    ConstantCondition(
        Substance* substance,
        double concentration
    ) : Condition(substance)
    {
        this->concentration = concentration;
    }

    virtual ~ConstantCondition()
    {}

    virtual double getConcentration()
    {
        return concentration;
    }

    virtual std::string getType()
    {
        return "CONSTANT";
    }
};


/**
 *  Reakcija ant elektrodo.
 *  TODO: Ar tikrai sito reikia????
 */
class Bound::ElectrodeCondition : public Bound::Condition
{
protected:

public:
    ElectrodeCondition(
        Substance* substance
    ) : Condition(substance)
    {}

    virtual ~ElectrodeCondition()
    {}

    virtual std::string getType()
    {
        return "ELECTRODE";
    }
};


/**
 *  Derinimo salyga.
 */
class Bound::MergeCondition : public Bound::Condition
{
protected:

public:
    MergeCondition(
        Substance* substance
    ) : Condition(substance)
    {}

    virtual ~MergeCondition()
    {}

    virtual std::string getType()
    {
        return "MERGE";
    }
};



/* ************************************************************************** */
/* ************************************************************************** */
}   // namespace cfg
#endif
