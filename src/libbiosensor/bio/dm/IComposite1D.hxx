#ifndef BIO_DM_IComposite1D_HXX
#define BIO_DM_IComposite1D_HXX
#include "../../biosensor.hxx"
#include "../Splitted1DArea.hxx"
#include "IGrid1D.hxx"
#include "IConcentrations.hxx"
BIO_DM_NS_BEGIN


typedef BIO_NS::Splitted1DArea<IGrid1D*, IConcentrations*> IComposite1D;


BIO_DM_NS_END
#endif
