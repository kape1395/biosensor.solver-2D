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
#ifndef BIO_Splitted1DArea_HXX
#define BIO_Splitted1DArea_HXX
#include "../biosensor.hxx"
#include <vector>
BIO_NS_BEGIN

/**
 *  Template for structure, that holds parts of splitted 1D linear area.
 *  Parameter A is class of area (sub-area).
 *  Parameter B is class of bounds, that surrounds sub-areas.
 */
template<class A, class B>
class Splitted1DArea
{
private:
    int size_;
    std::vector<A> areas_;
    std::vector<B> bounds_;
public:
    Splitted1DArea(int size)
    {
        size_ = size;
        areas_ .resize(size_    );
        bounds_.resize(size_ + 1);
    }
    virtual ~Splitted1DArea()
    {
        areas_.clear();
        bounds_.clear();
        size_ = 0;
    }
    inline int size() const
    {
        return size_;
    }
    inline std::vector<A>& areas()
    {
        return areas_;
    }
    inline std::vector<B>& bounds()
    {
        return bounds_;
    }
    inline A& area(int i)
    {
        return areas_[i];
    }
    inline B& bound(int i)
    {
        return bounds_[i];
    }
};


BIO_NS_END
#endif
