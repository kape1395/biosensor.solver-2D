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
#include "ConstantOnEdge.hxx"
#include <bio/Logging.hxx>
#define LOGGER "libbiosensor-slv-fd::im2d::ConstantOnEdge: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::ConstantOnEdge::ConstantOnEdge(int size, double value)
{
    this->size = size;
    this->value = value;

    LOG_DEBUG(LOGGER << "Created: constantValue=" << value);
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_FD_IM2D_NS::ConstantOnEdge::~ConstantOnEdge()
{
    //  Nothing to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
int BIO_SLV_FD_IM2D_NS::ConstantOnEdge::getSize()
{
    return size;
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_SLV_FD_IM2D_NS::ConstantOnEdge::getValue(int index)
{
    return value;
}


/* ************************************************************************** */
/* ************************************************************************** */
