#ifndef BIO_DM_IComposite2D_HXX
#define BIO_DM_IComposite2D_HXX
#include "../../biosensor.hxx"
#include "../Splitted2DArea.hxx"
#include "IGrid2D.hxx"
#include "IGrid1D.hxx"
#include "IConcentrations.hxx"
BIO_DM_NS_BEGIN


typedef BIO_NS::Splitted2DArea<IGrid2D*, IGrid1D*, IConcentrations*> IComposite2D;


BIO_DM_NS_END
#endif
