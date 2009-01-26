#ifndef BIO_Splitted2DArea_HXX
#define BIO_Splitted2DArea_HXX
#include "../biosensor.hxx"
#include <vector>
BIO_NS_BEGIN

/**
 *  Template for structure, that holds parts of splitted 2D rectangular area.
 *  Parameter A is class of area (sub-area).
 *  Parameter B is class of bounds, that surrounds sub-areas.
 *  Parameter C is class of corners, that exists at bound crossing points.
 */
template<class A, class B, class C>
class Splitted2DArea
{
private:
    int sizeH_;
    int sizeV_;
    std::vector< std::vector<A> > areas_;
    std::vector< std::vector<B> > boundsH_;
    std::vector< std::vector<B> > boundsV_;
    std::vector< std::vector<C> > corners_;
public:
    /* ********************************************************************** */
    /**
     *  Constructor.
     *  \param sizeH    Number of parts "horizontaly"
     *  \param sizeV    Number of parts "verticaly"
     */
    Splitted2DArea(int sizeH, int sizeV)
    {
        sizeH_ = sizeH;
        sizeV_ = sizeV;
        areas_  .resize(sizeH_    );
        boundsH_.resize(sizeH_    );
        boundsV_.resize(sizeH_ + 1);
        corners_.resize(sizeH_ + 1);
        for (int i = 0; i < sizeH_    ; areas_  [i++].resize(sizeV_    ));
        for (int i = 0; i < sizeH_    ; boundsH_[i++].resize(sizeV_ + 1));
        for (int i = 0; i < sizeH_ + 1; boundsV_[i++].resize(sizeV_    ));
        for (int i = 0; i < sizeH_ + 1; corners_[i++].resize(sizeV_ + 1));
    }
    /* ********************************************************************** */
    /**
     *  Destructor.
     */
    virtual ~Splitted2DArea()
    {
        for (int i = 0; i < sizeH_    ; areas_  [i++].clear());
        for (int i = 0; i < sizeH_    ; boundsH_[i++].clear());
        for (int i = 0; i < sizeH_ + 1; boundsV_[i++].clear());
        for (int i = 0; i < sizeH_ + 1; corners_[i++].clear());
        areas_  .clear();
        boundsH_.clear();
        boundsV_.clear();
        corners_.clear();
        sizeH_ = 0;
        sizeV_ = 0;
    }
    /* ********************************************************************** */
    /// Returns number of parts "horizintaly"
    inline int sizeH() const
    {
        return sizeH_;
    }
    /// Returns number of parts "verticaly"
    inline int sizeV() const
    {
        return sizeV_;
    }
    inline std::vector<std::vector<A> >& getAreas()
    {
        return areas_;
    }
    inline std::vector<std::vector<B> >& getBoundsH()
    {
        return boundsH_;
    }
    inline std::vector<std::vector<B> >& getBoundsV()
    {
        return boundsV_;
    }
    inline std::vector<std::vector<C> >& getCorners()
    {
        return corners_;
    }
    inline A& getArea(int x, int y)
    {
        return areas_[x][y];
    }
    inline B& getBoundH(int x, int y)
    {
        return boundsH_[x][y];
    }
    inline B& getBoundV(int x, int y)
    {
        return boundsV_[x][y];
    }
    inline C& getCorner(int x, int y)
    {
        return corners_[x][y];
    }
};

BIO_NS_END
#endif
