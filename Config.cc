#include "Config.hh"


cfg::Config::~Config()
{
    for ( ; substances     .size() > 0; delete *substances     .begin(), substances     .pop_front());
    for ( ; reactions      .size() > 0; delete *reactions      .begin(), reactions      .pop_front());
    for ( ; mediums        .size() > 0; delete *mediums        .begin(), mediums        .pop_front());
    for ( ; dimensionXParts.size() > 0; delete *dimensionXParts.begin(), dimensionXParts.pop_front());
    for ( ; dimensionYParts.size() > 0; delete *dimensionYParts.begin(), dimensionYParts.pop_front());
    for ( ; areas          .size() > 0; delete *areas          .begin(), areas          .pop_front());
    for ( ; bounds         .size() > 0; delete *bounds         .begin(), bounds         .pop_front());
}


cfg::Medium::~Medium()
{
    for ( ; diffusions.size() > 0; delete *diffusions.begin(), diffusions.pop_front());
}


cfg::Bound::~Bound()
{
    for ( ; conditions.size() > 0; delete *conditions.begin(), conditions.pop_front());
}

