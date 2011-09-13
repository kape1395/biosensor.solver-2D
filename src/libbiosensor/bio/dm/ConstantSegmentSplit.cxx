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
#include "ConstantSegmentSplit.hxx"


/* ************************************************************************** */
/* ************************************************************************** */
BIO_DM_NS::ConstantSegmentSplit::ConstantSegmentSplit(
    double start,
    double length,
    int stepCount
) : BIO_DM_NS::ISegmentSplit()
{
    this->start = start;
    this->length = length;
    this->stepCount = stepCount;
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_DM_NS::ConstantSegmentSplit::~ConstantSegmentSplit()
{
    //  Empty virtual destructor.
}


/* ************************************************************************** */
/* ************************************************************************** */
int BIO_DM_NS::ConstantSegmentSplit::getPointCount()
{
    return stepCount + 1;
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_DM_NS::ConstantSegmentSplit::getStartPosition()
{
    return start;
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_DM_NS::ConstantSegmentSplit::getLength()
{
    return length;
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_DM_NS::ConstantSegmentSplit::getLocalPointPosition(int i)
{
    return (length / stepCount) * i;
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_DM_NS::ConstantSegmentSplit::getStepSize(int i)
{
    return length / stepCount;
}


/* ************************************************************************** */
/* ************************************************************************** */
