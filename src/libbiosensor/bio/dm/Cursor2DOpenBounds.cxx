#include "Cursor2DOpenBounds.hxx"

/* ************************************************************************** */
/*
 *  Calculates size of the corresponding grid and stops at the upper left corner.
 */
BIO_DM_NS::Cursor2DOpenBounds::Cursor2DOpenBounds(ICursor2D* baseCursor, bool autoDelete)
        : cursor(baseCursor)
{
    this->cursor = baseCursor;
    this->autoDelete = autoDelete;
    sizeH = 0;
    sizeV = 0;
    posH = 0;
    posV = 0;
    cursor->colEnd();
    cursor->rowEnd();

    while (cursor->isValid())
    {
        cursor->left();
        sizeH++;
    }
    cursor->right();

    while (cursor->isValid())
    {
        cursor->top();
        sizeV++;
    }
    cursor->down();
}

/* ************************************************************************** */
BIO_DM_NS::Cursor2DOpenBounds::~Cursor2DOpenBounds()
{
    if (autoDelete)
    {
        delete cursor;
    }
}

/* ************************************************************************** */
void BIO_DM_NS::Cursor2DOpenBounds::left()
{
    cursor->left();
    posH--;
}

/* ************************************************************************** */
void BIO_DM_NS::Cursor2DOpenBounds::right()
{
    cursor->right();
    posH++;
}

/* ************************************************************************** */
void BIO_DM_NS::Cursor2DOpenBounds::top()
{
    cursor->top();
    posV--;
}

/* ************************************************************************** */
void BIO_DM_NS::Cursor2DOpenBounds::down()
{
    cursor->down();
    posV++;
}

/* ************************************************************************** */
void BIO_DM_NS::Cursor2DOpenBounds::rowStart()
{
    cursor->rowStart();
    posH = 0;
}

/* ************************************************************************** */
void BIO_DM_NS::Cursor2DOpenBounds::rowEnd()
{
    cursor->rowEnd();
    posH = sizeH - 1;
}

/* ************************************************************************** */
void BIO_DM_NS::Cursor2DOpenBounds::colStart()
{
    cursor->colStart();
    posV = 0;
}

/* ************************************************************************** */
void BIO_DM_NS::Cursor2DOpenBounds::colEnd()
{
    cursor->colEnd();
    posV = sizeV - 1;
}

/* ************************************************************************** */
bool BIO_DM_NS::Cursor2DOpenBounds::isValid()
{
    return cursor->isValid();
}

/* ************************************************************************** */
BIO_DM_NS::IConcentrations* BIO_DM_NS::Cursor2DOpenBounds::getConcentrations()
{
    bool onTop    = posV == 0;
    bool onBottom = posV == sizeV - 1;
    bool onLeft   = posH == 0;
    bool onRight  = posH == sizeH - 1;
    if (onTop || onBottom || onLeft || onRight))
    {
        if (onTop) cursor->down();
        if (onBottom) cursor->top();
        if (onLeft) cursor->right();
        if (onRight) cursor->left();
    }
    return cursor->getConcentrations();
}

/* ************************************************************************** */
