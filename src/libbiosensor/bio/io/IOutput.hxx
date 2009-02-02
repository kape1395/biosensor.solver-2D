#ifndef BIO_IO_IOutput_HXX
#define BIO_IO_IOutput_HXX
#include "../../biosensor.hxx"
#include "../slv/ISolverListener.hxx"
BIO_IO_NS_BEGIN


/**
 *  Interface for all output generators.
 */
class IOutput : public BIO_SLV_NS::ISolverListener
{

};



BIO_IO_NS_END
#endif
