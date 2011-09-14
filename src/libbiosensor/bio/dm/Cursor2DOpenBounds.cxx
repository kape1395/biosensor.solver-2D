/*
 * Copyright 2011 Karolis Petrauskas
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
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
    cursor->right();

    while (cursor->isValid())
    {
        cursor->top();
        sizeV++;
    }
    cursor->down();
    cursor->down();
}

/* ************************************************************************** */
BIO_DM_NS::Cursor2DOpenBounds::~Cursor2DOpenBounds()
{
    if (autoDelete)
        delete cursor;
}

/* ************************************************************************** */
void BIO_DM_NS::Cursor2DOpenBounds::left()
{
    posH--;
    if (posH > 0 && posH < sizeH - 2)
        cursor->left();
}

/* ************************************************************************** */
void BIO_DM_NS::Cursor2DOpenBounds::right()
{
    posH++;
    if (posH > 1 && posH < sizeH - 1)
        cursor->right();
}

/* ************************************************************************** */
void BIO_DM_NS::Cursor2DOpenBounds::top()
{
    posV--;
    if (posV > 0 && posV < sizeV - 2)
        cursor->top();
}

/* ************************************************************************** */
void BIO_DM_NS::Cursor2DOpenBounds::down()
{
    posV++;
    if (posV > 1 && posV < sizeV - 1)
        cursor->down();
}

/* ************************************************************************** */
void BIO_DM_NS::Cursor2DOpenBounds::rowStart()
{
    cursor->rowStart();
    cursor->right();
    posH = 0;
}

/* ************************************************************************** */
void BIO_DM_NS::Cursor2DOpenBounds::rowEnd()
{
    cursor->rowEnd();
    cursor->left();
    posH = sizeH - 1;
}

/* ************************************************************************** */
void BIO_DM_NS::Cursor2DOpenBounds::colStart()
{
    cursor->colStart();
    cursor->down();
    posV = 0;
}

/* ************************************************************************** */
void BIO_DM_NS::Cursor2DOpenBounds::colEnd()
{
    cursor->colEnd();
    cursor->top();
    posV = sizeV - 1;
}

/* ************************************************************************** */
bool BIO_DM_NS::Cursor2DOpenBounds::isValid()
{
    bool onTop    = posV < 0;
    bool onBottom = posV > sizeV - 1;
    bool onLeft   = posH < 0;
    bool onRight  = posH > sizeH - 1;
    return !(onTop || onBottom || onLeft || onRight) && cursor->isValid();
}

/* ************************************************************************** */
BIO_DM_NS::IConcentrations* BIO_DM_NS::Cursor2DOpenBounds::getConcentrations()
{
    return cursor->getConcentrations();
}

/* ************************************************************************** */
