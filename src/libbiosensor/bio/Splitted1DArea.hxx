#ifndef BIO_Splitted1DArea_HXX
#define BIO_Splitted1DArea_HXX
#include "../biosensor.hxx"
#include <vector>
BIO_NS_BEGIN

/**
 *  Template for structure, that holds parts of splitted 1D linear area.
 *  Parameter area is class of area (sub-area).
 *  Parameter bound is class of bounds, that surrounds sub-areas.
 */
template<class area, class bound>
class Splitted1DArea    // FIXME: Manau kad reiks ismesti...
{
private:
    int size_;
    std::vector<area> areas_;
    std::vector<bound> bounds_;
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
    inline std::vector<area>& areas()
    {
        return areas_;
    }
    inline std::vector<bound>& bounds()
    {
        return bounds_;
    }
    inline area& area(int i)
    {
        return areas_[i];
    }
    inline bound& bound(int i)
    {
        return bounds_[i];
    }
};


BIO_NS_END
#endif
