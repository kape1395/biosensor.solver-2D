#include "DebugSL.hxx"
#include "../Exception.hxx"
#include "../dm/IGrid2D.hxx"
#include "iostream"

/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::DebugSL::DebugSL(
    BIO_SLV_NS::ISolver* solver,
    std::ostream& output
) :
        log(log4cxx::Logger::getLogger("libbiosensor::DebugSL")),
        out(output)
{
    this->solver = solver;
    this->grid = dynamic_cast<BIO_DM_NS::IGrid2D*>(solver->getData());

    if (grid == 0)
    {
        throw Exception("DebugSL: IGrid2D Datamodel is required");
    }
    cursor = grid->newGridCursor();
}


/* ************************************************************************** */
/* ************************************************************************** */
BIO_IO_NS::DebugSL::~DebugSL()
{
    delete cursor;
}


/* ************************************************************************** */
/* ************************************************************************** */
void BIO_IO_NS::DebugSL::solveEventOccured()
{

    int substCount = grid->getSubstanceCount();

    out << "# h\tv";
    for (int s = 0; s < substCount; s++)
    {
        out << '\t' << grid->getSubstanceConf(s)->name();
    }
    out << std::endl;

    int h;
    int v;
    cursor->colStart();
    cursor->rowStart();
    for (v = 0; cursor->isValid(); v++, cursor->down())
    {
        for (h = 0; cursor->isValid(); h++, cursor->right())
        {
            BIO_DM_NS::IConcentrations* concentrations = cursor->getConcentrations();

            out << h << '\t' << v;
            for (int s = 0; s < substCount; s++)
            {
                out << '\t' << (*concentrations)[s];
            }
            out << std::endl;
        }
        cursor->rowStart();
    }

}


/* ************************************************************************** */
/* ************************************************************************** */
