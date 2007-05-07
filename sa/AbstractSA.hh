/// \file
/// \todo Nezinau dar tiksliai, kaip cia ka padaryti...
///
#ifndef SA_AbstractSA_HH
#define SA_AbstractSA_HH
#include "../dm/AbstractDM.hh"
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
    cfg::Config*                config;
    dm::Model*                  data;
    std::list<SolveListener*>   listeners;

    /// Iskviecia visus klausytojus.
    void invokeListeners()
    {
        std::list<SolveListener*>::iterator iterator = listeners.begin();
        for ( ; iterator != listeners.end() ; iterator++)
            (*iterator)->solveEventOccured(this);
    }

    /// Sita konstruktoriu gali kvieti tik sunines klases...
    Solver(cfg::Config* config, dm::Model* model)
    {
        this->config = config;
        this->data   = model;
    }

    /// Sita konstruktoriu gali kvieti tik sunines klases...
    Solver(cfg::Config* config)
    {
        this->config = config;
        this->data   = 0;
    }

public:

    /// Destruktorius.
    virtual ~Solver()
    {
        listeners.clear();
        if (data != 0)
        {
            delete data;
            data = 0;
        }
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
