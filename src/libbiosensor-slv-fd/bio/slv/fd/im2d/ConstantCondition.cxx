#include "ConstantCondition.hxx"


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::ConstantCondition::ConstantCondition(
    AreaSubSolver::EdgeData* edge,
    double concentration,
    bool atStart
)
{
    this->edge = edge;
    this->concentration = concentration;
    this->atStart = atStart;
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
    for (int i = 1; i < edge->getSize() - 1; i++)
    {
        edge->setC0(i, concentration);
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_FD_IM2D_NS::ConstantCondition::solveAlongForward()
{
    //  Nothing to do here.
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
