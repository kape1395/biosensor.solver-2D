#include "MergeCondition.hxx"


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::MergeCondition::MergeCondition(
    AreaSubSolver::EdgeData* edgePrev,
    AreaSubSolver::EdgeData* edgeNext,
    double diffusionPrev,
    double diffusionNext
) : log(log4cxx::Logger::getLogger("libbiosensor-slv-fd.im2d.MergeCondition"))
{
    LOG4CXX_DEBUG(log, "MergeCondition()");

    this->edgePrev = edgePrev;
    this->edgeNext = edgeNext;
    this->diffusionPrev = diffusionPrev;
    this->diffusionNext = diffusionNext;
    this->size = edgePrev->getSize(); // it is same as in #edgePrev

    a = diffusionPrev / edgePrev->getStepSize();
    c = diffusionNext / edgeNext->getStepSize();
    b = -(a + c);
    f = 0.0;
    applyInitialValues();
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
        double q0 = (f - a * edgePrev->getQ1(i)) / denominator;
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
    for (int i = 1; i < size - 1; i++)
    {
        //  NOTE: It is enough to write C to "prev" area only.
        double c0 = - (a * edgePrev->getC1(i) + c * edgeNext->getC1(i)) / b;
        edgePrev->setC0(i, c0);
        edgeNext->setC0(i, c0);
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::MergeCondition::applyInitialValues()
{
    solveAlongBackward();
}


/* ************************************************************************** */
/* ************************************************************************** */
