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
    ~Splitted1DArea()
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
