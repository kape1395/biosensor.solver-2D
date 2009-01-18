#include "ConstantCondition.hxx"


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::ConstantCondition::ConstantCondition(
    AreaSubSolver::EdgeData* edge,
    double concentration,
    bool atStart
) : log(log4cxx::Logger::getLogger("libbiosensor-slv-fd.im2d.ConstantCondition"))
{
    LOG4CXX_DEBUG(log, "ConstantCondition()");

    this->edge = edge;
    this->concentration = concentration;
    this->atStart = atStart;
    applyInitialValues();
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::ConstantCondition::~ConstantCondition()
{
    // Nothing to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::ConstantCondition::solveThroughForward()
{
    if (atStart) // P and Q are needed at start only
    {
        for (int i = 1; i < edge->getSize() - 1; i++)
        {
            edge->setP0(i, 0.0);
            edge->setQ0(i, concentration);
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::ConstantCondition::solveThroughBackward()
{
    if (atStart)
    {
        for (int i = 1; i < edge->getSize() - 1; i++)
        {
            edge->setC0(i, edge->getP0(i) * edge->getC1(i) + edge->getQ0(i));
        }
    }
    else
    {
        for (int i = 1; i < edge->getSize() - 1; i++)
        {
            edge->setC0(i, concentration);
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::ConstantCondition::solveAlongForward()
{
    //  Nothing to do here (for now the explicit aproach is used "along").
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::ConstantCondition::solveAlongBackward()
{
    for (int i = 1; i < edge->getSize() - 1; i++)
    {
        edge->setC0(i, concentration);
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::ConstantCondition::applyInitialValues()
{
    solveAlongBackward();
}


/* ************************************************************************** */
/* ************************************************************************** */
