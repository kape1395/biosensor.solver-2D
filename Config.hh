#ifndef CFG_Config_HH
#define CFG_Config_HH
#include <string>
#include <list>
namespace cfg
{



class Config;
class Substance;
class Reaction;
class Material;
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
 *      <material name="Enzyme">
 *          <diffusion substance="S" coefficient="1E-6"/>
 *          <diffusion substance="S" coefficient="1E-6"/>
 *          <reaction name="R1"/>
 *      </material>
 *      <material name="PerforatedMembrane">
 *      </material>
 *      <dimensionX>
 *          <part lenght="1E-3" type="Constant" stepCount="1000"/>
 *          <part lenght="1E-5" type="Bilinear" startStep="0.0001" factor="0.01"/>
 *      </dimensionX>
 *      <dimensionY>
 *          <part lenght="1E-5" type="Bilinear" startStep="0.0001" factor="0.01"/>
 *          <part lenght="1E-5" type="Constant" stepCount="1000"/>
 *      </dimensionY>
 *      <area position="0 0" material="Enzyme"/>
 *      <area position="1 0" material="PerforatedMembrane"/>
 *      <area position="0 1" material="Enzyme"/>
 *      <area position="1 1" material="Enzyme"/>
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
public:
    std::list<Substance*>       substances;
    std::list<Reaction*>        reactions;
    std::list<Material*>        materials;
    std::list<DimensionPart*>   dimensionXParts;
    std::list<DimensionPart*>   dimensionYParts;
    std::list<Area*>            areas;
    std::list<Bound*>           bounds;

public:
    Config();
    virtual ~Config();

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
    );
    virtual ~Substance();
    virtual std::string getName();

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
    );
    virtual ~Reaction();
    virtual std::string& getName();

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
    );
    virtual ~MichaelisMentenReaction();
    virtual Substance*  getSubstrate();
    virtual Substance*  getProduct();
    virtual double      getV_max();
    virtual double      getK_M();

};



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Medziaga, esanti kiokioje nors biojutiklio srityje.
 */
class Material
{
public:
    class Diffusion;
    class Reaction;

protected:
    std::string             name;
    std::list<Diffusion*>   diffusions;
    std::list<Reaction*>    reactions;

public:
    Material(
        std::string name
    );
    virtual ~Material();
    virtual std::string getName();
    virtual std::list<Diffusion*> getDiffusions();
    virtual std::list<Reaction*> getReactions();
};


/**
 *  Substancijos difuzija medziagoje.
 */
class Material::Diffusion
{
protected:
    std::string substance;
    double      coefficient;

public:
    Diffusion(
        std::string substance,
        double      coefficient
    );
    virtual ~Diffusion();
    virtual std::string getSubstance();
    virtual double      getCoefficient();

};


/**
 *  Reakcija vykstanti medziagoje.
 */
class Material::Reaction
{
protected:
    std::string substance;

public:
    Reaction(
        std::string name
    );
    virtual ~Reaction();
    virtual std::string getName();

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
    );
    virtual ~DimensionPart();
    virtual double getLength();
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
    ConstantDimensionPart(
        double length,
        int    stepCount
    );
    virtual ~ConstantDimensionPart();
    virtual int getStepCount();
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
    );
    virtual ~BilinearDimensionPart();
    virtual double getStartStep();
    virtual double getFactor();

};



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Homogeniska biojutiklio sritis.
 */
class Area
{
protected:
    int         position[2];
    std::string material;

public:
    Area(
        int         position[2],
        std::string material
    );
    virtual ~Area();
    virtual int* getPosition();
    virtual std::string getMaterial();

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
    enum SIDE {top, right, bottom, left};

protected:
    int position[3];    // x, y, SIDE
    std::list<Condition*> conditions;

public:
    Bound(
        int positions[3]
    );
    virtual ~Bound();
    virtual int* getPosition();
    virtual std::list<Condition*> getConditions();

};


/**
 *  Konkrecios medziagos elgesys srities kraste.
 */
class Bound::Condition
{
protected:
    std::string substance;

public:
    Condition(
        std::string substance
    );
    virtual ~Condition();
    virtual std::string getSubstance();

};


/**
 *  Nepratekejimo salyga.
 */
class Bound::WallCondition : public Bound::Condition
{
protected:

public:
    WallCondition(
        std::string substance
    );
    virtual ~WallCondition();

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
        std::string substance,
        double concentration
    );
    virtual ~ConstantCondition();
    virtual double getConcentration();

};


/**
 *  Reakcija ant elektrodo.
 */
class Bound::ElectrodeCondition : public Bound::Condition
{
protected:

public:
    ElectrodeCondition(
        std::string substance
    );
    virtual ~ElectrodeCondition();

};


/**
 *  Derinimo salyga.
 */
class Bound::MergeCondition : public Bound::Condition
{
protected:

public:
    MergeCondition(
        std::string substance
    );
    virtual ~MergeCondition();

};



/* ************************************************************************** */
/* ************************************************************************** */
}   // namespace cfg
#endif
