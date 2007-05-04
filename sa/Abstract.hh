/// \file
/// \todo Nezinau dar tiksliai, kaip cia ka padaryti...
///
#ifndef SA_Abstract_HH
#define SA_Abstract_HH
#include "../dm/Abstract.hh"
#include <list>


namespace sa
{
/* ************************************************************************** */
/* ************************************************************************** */
class SolveListener;
class Solver;



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Interfeisas, kuri turi realizuoti klases, kurios nori gauti
 *  modeliavimo ivykius.
 */
class SolveListener
{
public:
    virtual ~SolveListener()
    {}
    virtual void solveEventOccured(Solver *solver) = 0;
};



/* ************************************************************************** */
/* ************************************************************************** */
/**
 *  Sprendejo bazine klase.
 */
class Solver
{
protected:
    dm::Model*                  data;
    std::list<SolveListener*>   listeners;

    /// Iskviecia visus klausytojus.
    void invokeListeners()
    {
        std::list<SolveListener*>::iterator iterator = listeners.begin();
        for ( ; iterator != listeners.end() ; iterator++)
            (*iterator)->solveEventOccured(this);
    }

public:

    /// Konstruktorius.
    Solver(dm::Model* data)
    {
        this->data = data;
    }

    /// Destruktorius.
    virtual ~Solver()
    {
        listeners.clear();
    }

    /// Cia vyksta modeliavimas.
    virtual void solve() = 0;

    /// Prideti sprendimo ivykiu klausytoja.
    virtual void addListener(SolveListener *listener)
    {
        listeners.push_back(listener);
    }

    /// Grazina sprendejo duomenu modeli.
    virtual dm::Model* getData()
    {
        return data;
    }

};



/* ************************************************************************** */
/* ************************************************************************** */
}   // namespace sa
#endif
