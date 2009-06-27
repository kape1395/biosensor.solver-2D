#include "Cursor2DWithoutBounds.hxx"

/* ************************************************************************** */
BIO_DM_NS::Cursor2DWithoutBounds::Cursor2DWithoutBounds(ICursor2D& baseCursor)
        : cursor(baseCursor)
{
    updateValidity();
}

/* ************************************************************************** */
BIO_DM_NS::Cursor2DWithoutBounds::~Cursor2DWithoutBounds()
{
    //  Empty virtual destructor.
}

/* ************************************************************************** */
void BIO_DM_NS::Cursor2DWithoutBounds::left()
{
    cursor.left();
    updateValidity();
}

/* ************************************************************************** */
void BIO_DM_NS::Cursor2DWithoutBounds::right()
{
    cursor.right();
    updateValidity();
}

/* ************************************************************************** */
void BIO_DM_NS::Cursor2DWithoutBounds::top()
{
    cursor.top();
    updateValidity();
}

/* ************************************************************************** */
void BIO_DM_NS::Cursor2DWithoutBounds::down()
{
    cursor.down();
    updateValidity();
}

/* ************************************************************************** */
void BIO_DM_NS::Cursor2DWithoutBounds::rowStart()
{
    cursor.rowStart();
    cursor.right();
    updateValidity();
}

/* ************************************************************************** */
void BIO_DM_NS::Cursor2DWithoutBounds::rowEnd()
{
    cursor.rowEnd();
    cursor.left();
    updateValidity();
}

/* ************************************************************************** */
void BIO_DM_NS::Cursor2DWithoutBounds::colStart()
{
    cursor.colStart();
    cursor.down();
    updateValidity();
}

/* ************************************************************************** */
void BIO_DM_NS::Cursor2DWithoutBounds::colEnd()
{
    cursor.colEnd();
    cursor.top();
    updateValidity();
}

/* ************************************************************************** */
bool BIO_DM_NS::Cursor2DWithoutBounds::isValid()
{
    return valid;
}

void BIO_DM_NS::Cursor2DWithoutBounds::updateValidity() {
    valid = cursor.isValid();
    
    if (!valid) return;

    cursor.right();
    valid = valid && cursor.isValid();
    cursor.left();
    if (!valid) return;

    cursor.left();
    valid = valid && cursor.isValid();
    cursor.right();
    if (!valid) return;

    cursor.top();
    valid = valid && cursor.isValid();
    cursor.down();
    if (!valid) return;

    cursor.down();
    valid = valid && cursor.isValid();
    cursor.top();
    if (!valid) return;
}

/* ************************************************************************** */
BIO_DM_NS::IConcentrations* BIO_DM_NS::Cursor2DWithoutBounds::getConcentrations()
{
    return valid ? cursor.getConcentrations() : 0;
}

/* ************************************************************************** */
