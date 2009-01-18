#include "WallCondition.hxx"


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::WallCondition::WallCondition(
    AreaSubSolver::EdgeData* edge,
    bool atStart
) : log(log4cxx::Logger::getLogger("libbiosensor-slv-fd::im2d::WallCondition"))
{
    LOG4CXX_DEBUG(log, "WallCondition()");

    this->edge = edge;
    this->atStart = atStart;

    applyInitialValues();
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::WallCondition::~WallCondition()
{
    //  Nothing to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::WallCondition::solveThroughForward()
{
    if (atStart)    // P and Q are needed at start only
    {
        for (int i = 1; i < edge->getSize() - 1; i++)
        {
            edge->setP0(i, 1.0);
            edge->setQ0(i, 0.0);
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::WallCondition::solveThroughBackward()
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
            edge->setC0(i, -(edge->getQ1(i) / (edge->getP1(i) - 1)));
        }
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::WallCondition::solveAlongForward()
{
    //  Nothing to do here (for now the explicit aproach is used "along").
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::WallCondition::solveAlongBackward()
{
    for (int i = 1; i < edge->getSize() - 1; i++)
    {
        edge->setC0(i, edge->getC1(i));
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::WallCondition::applyInitialValues()
{
    solveAlongBackward();
}


/* ************************************************************************** */
/* ************************************************************************** */
