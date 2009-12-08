#include "CompositeElectrode.hxx"
#include "../Logging.hxx"
#include "../Exception.hxx"
#define LOGGER "libbiosensor::CompositeElectrode: "


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::CompositeElectrode::CompositeElectrode()
{
    //  Nothing to do here.
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_TRD_NS::CompositeElectrode::~CompositeElectrode()
{
    std::vector<BIO_SLV_NS::ITransducer*>::iterator it;
    for (it = transducersToDelete.begin(); it < transducersToDelete.end(); it++)
    {
        delete *it;
    }
    transducers.clear();
    transducersToDelete.clear();
}

/* ************************************************************************** */
/* ************************************************************************** */
void BIO_TRD_NS::CompositeElectrode::addTransducer(BIO_SLV_NS::ITransducer* transducer, bool deleteOnDestruction)
{
    transducers.push_back(transducer);
    if (deleteOnDestruction)
    {
        transducersToDelete.push_back(transducer);
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
double BIO_TRD_NS::CompositeElectrode::getOutput()
{
    double result = 0.0;
    std::vector<BIO_SLV_NS::ITransducer*>::iterator it;
    for (it = transducersToDelete.begin(); it < transducersToDelete.end(); it++)
    {
        result += (*it)->getOutput();
    }
    return result;
}


/* ************************************************************************** */
/* ************************************************************************** */
