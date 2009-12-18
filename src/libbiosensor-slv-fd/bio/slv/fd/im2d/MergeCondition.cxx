#include "MergeCondition.hxx"
#include <string>
#include <bio/Logging.hxx>
#define LOGGER "libbiosensor-slv-fd::im2d::MergeCondition: "

/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::MergeCondition::MergeCondition(
    IAreaEdgeData* edgePrev,
    IAreaEdgeData* edgeNext,
    double diffusionPrev,
    double diffusionNext,
    IAreaEdgeFunction* function
)
{
    this->edgePrev = edgePrev;
    this->edgeNext = edgeNext;
    this->diffusionPrev = diffusionPrev;
    this->diffusionNext = diffusionNext;
    this->function = function;
    this->size = edgePrev->getSize(); // it is same as in #edgePrev

    if (diffusionPrev == 0 && diffusionNext == 0)   //  FIXME: Solve this issue somehow... maybe using bound condition implemented by area solver.
    {
        LOG_WARN(LOGGER << "I dont know now what to do on bounds with both sides have diffusion 0, so...");
        this->diffusionPrev = 1.0;
        this->diffusionNext = 1.0;
    }

    a = this->diffusionPrev / edgePrev->getStepSize();
    c = this->diffusionNext / edgeNext->getStepSize();
    b = -(a + c);
    //f = 0.0;      --  function will be used instead of this.
    applyInitialValues();

    LOG_DEBUG(LOGGER << "Created: diffusionPrev=" << diffusionPrev << " diffusionNext=" << diffusionNext << ")");
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::MergeCondition::~MergeCondition()
{
    //  Nothing to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::MergeCondition::solveThroughForward()
{
    for (int i = 1; i < size - 1; i++)
    {
        //  NOTE: It is enough to write P and Q to "next" area only.
        double denominator = a * edgePrev->getP1(i) + b;
        double p0 = - c / denominator;
        double q0 = (function->getValue(i) - a * edgePrev->getQ1(i)) / denominator;
        edgePrev->setP0(i, p0);
        edgeNext->setP0(i, p0);
        edgePrev->setQ0(i, q0);
        edgeNext->setQ0(i, q0);
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::MergeCondition::solveThroughBackward()
{
    for (int i = 1; i < size - 1; i++)
    {
        //  NOTE: It is enough to write C to "prev" area only.
        double c0 = edgeNext->getP0(i) * edgeNext->getC1(i) + edgeNext->getQ0(i);
        edgePrev->setC0(i, c0);
        edgeNext->setC0(i, c0);
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  NOTE: "Solve Along" is done using explicit scheme.
 */
void BIO_SLV_FD_IM2D_NS::MergeCondition::solveAlongForward()
{
    //  Nothing to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  NOTE: "Solve Along" is done using explicit scheme.
 */
void BIO_SLV_FD_IM2D_NS::MergeCondition::solveAlongBackward()
{
    //for (int i = 1; i < size - 1; i++)
    for (int i = 0; i < size; i++)
    {
        //  NOTE: It is enough to write C to "prev" area only.
        double c0 = function->getValue(i) - (a * edgePrev->getC1(i) + c * edgeNext->getC1(i)) / b;
        edgePrev->setC0(i, c0);
        edgeNext->setC0(i, c0);
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::MergeCondition::applyInitialValues()
{
    // solveAlongBackward();
    //
    //  FIXME: We cant call function here, so using 0.0 instead.
    //
    for (int i = 0; i < size; i++)
    {
        double c0 = 0.0 - (a * edgePrev->getC1(i) + c * edgeNext->getC1(i)) / b;
        edgePrev->setC0(i, c0);
        edgeNext->setC0(i, c0);
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_SLV_FD_IM2D_NS::MergeCondition::getConcentration(int x)
{
    return edgePrev->getC0(x);
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::MergeCondition::setConcentration(int x, double c)
{
    edgePrev->setC0(x, c);
    edgeNext->setC0(x, c);
}


/* ************************************************************************** */
/* ************************************************************************** */
