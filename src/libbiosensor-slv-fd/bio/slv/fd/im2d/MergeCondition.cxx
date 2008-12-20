#include "MergeCondition.hxx"


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::MergeCondition::MergeCondition(
    AreaSubSolver::EdgeData* edgePrev,
    AreaSubSolver::EdgeData* edgeNext,
    double diffusionPrev,
    double diffusionNext
)
{
    this->edgePrev = edgePrev;
    this->edgeNext = edgeNext;
    this->diffusionPrev = diffusionPrev;
    this->diffusionNext = diffusionNext;

    a = diffusionPrev / edgePrev->getStepSize();
    c = diffusionNext / edgeNext->getStepSize();
    b = -(a + c);
    f = 0.0;
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
        edgeNext->setP0(i, - c / denominator);
        edgeNext->setQ0(i, (f - a * edgePrev->getQ1(i)) / denominator);
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::MergeCondition::solveThroughBackward()
{
    for (int i = 1; i < size - 1; i++)
    {
        //  NOTE: It is enough to write C to "prev" area only.
        edgePrev->setC0(i, edgeNext->getP0(i) * edgeNext->getC1(i) + edgeNext->getQ0(i));
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
    for (int i = 1; i < size - 1; i++)
    {
        //  NOTE: It is enough to write C to "prev" area only.
        edgePrev->setC0(i, - (a * edgePrev->getC1(i) + c * edgeNext->getC1(i)) / b);
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
