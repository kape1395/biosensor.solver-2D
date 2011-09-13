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
#include "NullSolver.hxx"


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::NullSolver::NullSolver(
    BIO_XML_NS::model::Model* config
) : BIO_SLV_NS::AbstractSolver(config)
{
    data = new BIO_DM_NS::NullDM();
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_SLV_NS::NullSolver::~NullSolver()
{
    delete data;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_DM_NS::IDataModel* BIO_SLV_NS::NullSolver::getData()
{
    return data;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_SLV_NS::NullSolver::solve()
{
    // Nothing to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
